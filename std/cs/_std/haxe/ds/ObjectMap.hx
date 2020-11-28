/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.ds;

import cs.NativeArray;

@:coreApi class ObjectMap<K:{}, V> implements haxe.Constraints.IMap<K, V> {
	extern private static inline var HASH_UPPER = 0.77;
	extern private static inline var FLAG_EMPTY = 0;
	extern private static inline var FLAG_DEL = 1;

	/**
	 * This is the most important structure here and the reason why it's so fast.
	 * It's an array of all the hashes contained in the table. These hashes cannot be 0 nor 1,
	 * which stand for "empty" and "deleted" states.
	 *
	 * The lookup algorithm will keep looking until a 0 or the key wanted is found;
	 * The insertion algorithm will do the same but will also break when FLAG_DEL is found;
	 */
	private var hashes:NativeArray<HashType>;

	private var _keys:NativeArray<K>;
	private var vals:NativeArray<V>;

	private var nBuckets:Int;
	private var size:Int;
	private var nOccupied:Int;
	private var upperBound:Int;

	#if !no_map_cache
	private var cachedKey:K;
	private var cachedIndex:Int;
	#end

	#if DEBUG_HASHTBL
	private var totalProbes:Int;
	private var probeTimes:Int;
	private var sameHash:Int;
	private var maxProbe:Int;
	#end

	public function new():Void {
		#if !no_map_cache
		cachedIndex = -1;
		#end
	}

	public function set(key:K, value:V):Void {
		var x:Int, k:Int;
		if (nOccupied >= upperBound) {
			if (nBuckets > (size << 1))
				resize(nBuckets - 1); // clear "deleted" elements
			else
				resize(nBuckets + 2);
		}

		var hashes = hashes, keys = _keys, hashes = hashes;
		{
			var mask = (nBuckets == 0) ? 0 : nBuckets - 1;
			var site = x = nBuckets;
			k = hash(key);
			var i = k & mask, nProbes = 0;

			var delKey = -1;
			// for speed up
			if (isEmpty(hashes[i])) {
				x = i;
			} else {
				// var inc = getInc(k, mask);
				var last = i, flag;
				while (!(isEmpty(flag = hashes[i]) || (flag == k && _keys[i] == key))) {
					if (isDel(flag) && delKey == -1)
						delKey = i;
					i = (i + ++nProbes) & mask;
					#if DEBUG_HASHTBL
					probeTimes++;
					if (i == last)
						throw "assert";
					#end
				}

				if (isEmpty(flag) && delKey != -1)
					x = delKey;
				else
					x = i;
			}

			#if DEBUG_HASHTBL
			if (nProbes > maxProbe)
				maxProbe = nProbes;
			totalProbes++;
			#end
		}

		var flag = hashes[x];
		if (isEmpty(flag)) {
			keys[x] = key;
			vals[x] = value;
			hashes[x] = k;
			size++;
			nOccupied++;
		} else if (isDel(flag)) {
			keys[x] = key;
			vals[x] = value;
			hashes[x] = k;
			size++;
		} else {
			assert(_keys[x] == key);
			vals[x] = value;
		}

		#if !no_map_cache
		cachedIndex = x;
		cachedKey = key;
		#end
	}

	private final function lookup(key:K):Int {
		if (nBuckets != 0) {
			var hashes = hashes, keys = _keys;

			var mask = nBuckets - 1, hash = hash(key), k = hash, nProbes = 0;
			var i = k & mask;
			var last = i, flag;
			// var inc = 
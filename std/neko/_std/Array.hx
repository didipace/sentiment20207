
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

import haxe.iterators.ArrayKeyValueIterator;

@:coreApi final class Array<T> {
	private var __a:neko.NativeArray<T>;

	public var length(default, null):Int;

	public function new():Void {
		this.__a = neko.NativeArray.alloc(0);
		this.length = 0;
	}

	private static function new1<T>(a:neko.NativeArray<T>, l:Int):Array<T> {
		var inst = new Array<T>();
		inst.__a = a;
		inst.length = l;
		return inst;
	}

	public function concat(a:Array<T>):Array<T> {
		var a1 = this.__a;
		var a2 = a.__a;
		var s1 = this.length;
		var s2 = a.length;
		var a = neko.NativeArray.alloc(s1 + s2);
		neko.NativeArray.blit(a, 0, a1, 0, s1);
		neko.NativeArray.blit(a, s1, a2, 0, s2);
		return new1(a, s1 + s2);
	}

	public function copy():Array<T> {
		return new1(neko.NativeArray.sub(this.__a, 0, this.length), this.length);
	}

	public inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator(this);
	}

	public inline function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator(this);
	}

	public function insert(pos:Int, x:T):Void {
		var l = this.length;
		if (pos < 0) {
			pos = l + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos > l)
			pos = l;
		this.__grow(l + 1);
		var a = this.__a;
		neko.NativeArray.blit(a, pos + 1, a, pos, l - pos);
		a[pos] = x;
	}

	public function join(sep:String):String {
		var s = new StringBuf();
		var a = this.__a;
		var max = this.length - 1;
		for (p in 0...this.length) {
			s.add(a[p]);
			if (p != max)
				s.add(sep);
		}
		return s.toString();
	}

	static var __hx_toString_depth = 0;

	public function toString():String {
		if (__hx_toString_depth >= 5) {
			return "...";
		}
		var s = new StringBuf();
		s.add("[");
		var it = iterator();
		__hx_toString_depth++;
		try {
			for (i in it) {
				s.add(i);
				if (it.hasNext())
					s.addChar(",".code);
			}
			__hx_toString_depth--;
		} catch (e:Dynamic) {
			__hx_toString_depth--;
			neko.Lib.rethrow(e);
		}
		s.add("]");
		return s.toString();
	}

	public function pop():Null<T> {
		if (this.length == 0)
			return null;
		this.length -= 1;
		var x = this.__a[this.length];
		this.__a[this.length] = null;
		return x;
	}

	public function push(x:T):Int {
		var l = this.length;
		this.__grow(l + 1);
		this.__a[l] = x;
		return l + 1;
	}

	public function unshift(x:T):Void {
		var l = this.length;
		this.__grow(l + 1);
		var a = this.__a;
		neko.NativeArray.blit(a, 1, a, 0, l);
		a[0] = x;
	}

	public function remove(x:T):Bool {
		var i = 0;
		var l = this.length;
		var a = this.__a;
		while (i < l) {
			if (a[i] == x) {
				neko.NativeArray.blit(a, i, a, i + 1, l - i - 1);
				l -= 1;
				this.length = l;
				a[l] = null;
				return true;
			}
			i += 1;
		}
		return false;
	}

	public function contains(x:T):Bool {
		var i = 0;
		var l = this.length;
		var a = this.__a;
		while (i < l) {
			if (a[i] == x) {
				return true;
			}
			i += 1;
		}
		return false;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		var len = length;
		var i:Int = (fromIndex != null) ? fromIndex : 0;
		var a = __a;
		if (i < 0) {
			i += len;
			if (i < 0)
				i = 0;
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

import cs.NativeArray;
import haxe.iterators.ArrayKeyValueIterator;

#if core_api_serialize
@:meta(System.Serializable)
#end
final class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;

	private var __a:NativeArray<T>;

	@:skipReflection static var __hx_toString_depth = 0;
	@:skipReflection static inline final __hx_defaultCapacity = 4;

	#if erase_generics
	inline private static function ofNative<X>(native:NativeArray<Dynamic>):Array<X> {
		return new Array(native);
	}
	#else
	inline private static function ofNative<X>(native:NativeArray<X>):Array<X> {
		return new Array(native);
	}
	#end

	inline private static function alloc<Y>(size:Int):Array<Y> {
		return new Array(new NativeArray(size));
	}

	@:overload public function new():Void {
		this.length = 0;
		this.__a = new NativeArray(0);
	}

	#if erase_generics
	@:overload private function new(native:NativeArray<Dynamic>) {
		this.length = native.Length;
		this.__a = untyped native;
	}
	#else
	@:overload private function new(native:NativeArray<T>) {
		this.length = native.Length;
		this.__a = native;
	}
	#end

	public function concat(a:Array<T>):Array<T> {
		var len = length + a.length;
		var retarr = new NativeArray(len);
		cs.system.Array.Copy(__a, 0, retarr, 0, length);
		cs.system.Array.Copy(a.__a, 0, retarr, length, a.length);

		return ofNative(retarr);
	}

	private function concatNative(a:NativeArray<T>):Void {
		var __a = __a;
		var len = length + a.Length;
		if (__a.Length >= len) {
			cs.system.Array.Copy(a, 0, __a, length, length);
		} else {
			var newarr = new NativeArray(len);
			cs.system.Array.Copy(__a, 0, newarr, 0, length);
			cs.system.Array.Copy(a, 0, newarr, length, a.Length);

			this.__a = newarr;
		}

		this.length = len;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		var len = length, i:Int = (fromIndex == null) ? 0 : fromIndex;
		if (i < 0) {
			i += len;
			if (i < 0)
				i = 0;
		} else if (i >= len) {
			return -1;
		}
		return cs.system.Array.IndexOf(__a, x, i, len - i);
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		var len = length, i:Int = (fromIndex == null) ? len - 1 : fromIndex;
		if (i >= len) {
			i = len - 1;
		} else if (i < 0) {
			i += len;
			if (i < 0)
				return -1;
		}
		return cs.system.Array.LastIndexOf(__a, x, i, i + 1);
	}

	public function join(sep:String):String {
		var buf = new StringBuf();
		var i = -1;

		var first = true;
		var length = length;
		while (++i < length) {
			if (first)
				first = false;
			else
				buf.add(sep);
			buf.add(__a[i]);
		}

		return buf.toString();
	}

	public function pop():Null<T> {
		var __a = __a;
		var length = length;
		if (length > 0) {
			var val = __a[--length];
			__a[length] = null;
			this.length = length;

			return val;
		} else {
			return null;
		}
	}

	public function push(x:T):Int {
		if (length >= __a.Length) {
			var newLen = length == 0 ? __hx_defaultCapacity : (length << 1);
			var newarr = new NativeArray(newLen);
			__a.CopyTo(newarr, 0);

			this.__a = newarr;
		}

		__a[length] = x;
		r
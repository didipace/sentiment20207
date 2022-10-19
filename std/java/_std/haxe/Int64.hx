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

package haxe;

using haxe.Int64;

import haxe.Int64Helper;

private typedef __Int64 = java.StdTypes.Int64;

@:coreApi
@:transitive
abstract Int64(__Int64) from __Int64 to __Int64 {
	#if jvm
	extern public static function make(high:Int32, low:Int32):Int64;
	#else
	public static inline function make(high:Int32, low:Int32):Int64
		return new Int64(((cast high : __Int64) << 32) | ((cast low : __Int64) & (untyped __java__('0xffffffffL') : Int64)));
	#end

	private inline function new(x:__Int64)
		this = x;

	private var val(get, set):__Int64;

	inline function get_val():__Int64
		return this;

	inline function set_val(x:__Int64):__Int64
		return this = x;

	public var high(get, never):Int32;

	inline function get_high():Int32
		return cast(this >> 32);

	public var low(get, never):Int32;

	inline function get_low():Int32
		return cast this;

	public inline function copy():Int64
		return new Int64(this);

	@:from public static inline function ofInt(x:Int):Int64
		return cast x;

	@:deprecated('haxe.Int64.is() is deprecated. Use haxe.Int64.isInt64() instead')
	inline public static function is(val:Dynamic):Bool
		return Std.isOfType(val, java.lang.Long.LongClass);

	inline public static function isInt64(val:Dynamic):Bool
		return Std.isOfType(val, java.lang.Long.LongClass);

	public static inline function toInt(x:Int64):Int {
		if (x.val < 0x80000000 || x.val > 0x7FFFFFFF)
			throw "Overflow";
		return cast x.val;
	}

	public static inline function getHigh(x:Int64):Int32
		return cast(x.val >> 32);

	public static inli
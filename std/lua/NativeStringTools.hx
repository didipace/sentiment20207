
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

package lua;

/**
	These are all externs for the base Lua "string" class, which functions
	as an additional set of string tools.

	Note that all relevant indexes are "1" based.
**/
@:native("_G.string")
extern class NativeStringTools {
	/**
		Receives a string and returns its length. The empty string `""` has
		length `0`. Embedded zeros are counted, so `"a\000bc\000"` has length `5`.
	**/
	static function len(str:String):Int;

	/**
		Receives zero or more integers. Returns a string with length equal to the
		number of arguments, in which each character has the internal numerical
		code equal to its corresponding argument.
		Note that numerical codes are not necessarily portable across platforms.
	**/
	static function char(codes:haxe.extern.Rest<Int>):String;

	// TODO: make a note about handling matched groups with multireturn

	/**
		Returns the substring of `str` that starts at `start` and continues until `end`;
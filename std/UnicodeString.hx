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

import haxe.io.Bytes;
import haxe.io.Encoding;
import haxe.iterators.StringIteratorUnicode;
import haxe.iterators.StringKeyValueIteratorUnicode;

/**
	This abstract provides consistent cross-target unicode support for characters of any width.

	Due to differing internal representations of strings across targets, only the basic
	multilingual plane (BMP) is supported consistently by `String` class.

	This abstract provides API to consistently handle all characters even beyond BMP.

	@see https://haxe.org/manual/std-String-unicode.html
**/
@:forward
@:access(StringTools)
abstract UnicodeString(String) from String to String {
	/**
		Tells if `b` is a correctly encoded UTF8 byte sequence.
	**/
	static public function validate(b:Bytes, encoding:Encoding):Bool {
		switch (encoding) {
			case RawNative:
				throw "UnicodeString.validate: RawNative encoding is not supported";
			case UTF8:
				var data = b.getData();
				var pos = 0;
				var max = b.length;
				while (pos < max) {
					var c:Int = Bytes.fastGet(data, pos++);
					if (c < 0x80) {} else if (c < 0xC2) {
						return false;
					} else if (c < 0xE0) {
						if (pos + 1 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c2 < 0x80 || c2 > 0xBF) {
							return false;
						}
					} else if (c < 0xF0) {
						if (pos + 2 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c == 0xE0) {
							if (c2 < 0xA0 || c2 > 0xBF)
								return false;
						} else {
							if (c2 < 0x80 || c2 > 0xBF)
								return false;
						}
						var c3:Int = Bytes.fastGet(data, pos++);
						if (c3 < 0x80 || c3 > 0xBF) {
							return false;
						}
						c = (c << 16) | (c2 << 8) | c3;
						if (0xEDA080 <= c && c <= 0xEDBFBF) { // surrogate pairs
							return false;
						}
					} else if (c > 0xF4) {
						return false;
					} else {
						if (pos + 3 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c == 0xF0) {
							if (c2 < 0x90 || c2 > 0xBF)
								return false;
						} else if (c == 0xF4) {
							if (c2 < 0x80 || c2 > 0x8F)
								return false;
						} else {
							if (c2 < 0x80 || c2 > 0xBF)
								return false;
						}
						var c3:Int = Bytes.fastGet(data, pos++);
						if (c3 < 0x80 || c3 > 0xBF) {
							return false;
						}
						var c4:Int = Bytes.fastGet(data, pos++);
						if (c4 < 0x80 || c4 > 0xBF) {
							return false;
						}
					}
				}
				return true;
		}
	}

	#if target.unicode
	/**
		Creates an instance of UnicodeString.
	**/
	public inline function new(string:String):Void {
		this = string;
	}

	/**
		Returns an iterator of the unicode code points.
	**/
	public inline function iterator():StringIteratorUnicode {
		return new StringIteratorUnicode(this);
	}

	/**
		Returns 
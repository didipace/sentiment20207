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

package haxe.io;

/**
	An Input is an abstract reader. See other classes in the `haxe.io` package
	for several possible implementations.

	All functions which read data throw `Eof` when the end of the stream
	is reached.
**/
class Input {
	/**
		Endianness (word byte order) used when reading numbers.

		If `true`, big-endian is used, otherwise `little-endian` is used.
	**/
	public var bigEndian(default, set):Bool;

	#if cs
	private var helper:BytesData;
	#elseif java
	private var helper:java.nio.ByteBuffer;
	#end

	/**
		Read and return one byte.
	**/
	public function readByte():Int {
		#if cpp
		throw new haxe.exceptions.NotImplementedException();
		#else
		return throw new haxe.exceptions.NotImplementedException();
		#end
	}

	/**
		Read `len` bytes and write them into `s` to the position specified by `pos`.

		Returns the actual length of read data that can be smaller than `len`.

		See `readFullBytes` that tries to read the exact amount of specified bytes.
	**/
	public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var k = len;
		var b = #if (js || hl) @:privateAccess s.b #else s.getData() #end;
		if (pos < 0 || len < 0 || pos + len > s.length)
			throw Error.OutsideBounds;
		try {
			while (k > 0) {
				#if neko
				untyped __dollar__sset(b, pos, readByte());
				#elseif php
				b.set(pos, readByte());
				#elseif cpp
				b[pos] = untyped readByte();
				#else
				b[pos] = cast readByte();
				#end
				pos++;
				k--;
			}
		} catch (eof:haxe.io.Eof) {}
		return l
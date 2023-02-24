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

package haxe.zip;

import haxe.ds.List;

class Writer {
	/**
		The next constant is required for computing the Central
		Directory Record(CDR) size. CDR consists of some fields
		of constant size and a filename. Constant represents
		total length of all fields with constant size for each
		file in archive
	**/
	inline static var CENTRAL_DIRECTORY_RECORD_FIELDS_SIZE = 46;

	/**
		The following constant is the total size of all fields
		of Local File Header. It's required for calculating
		offset of start of central directory record
	**/
	inline static var LOCAL_FILE_HEADER_FIELDS_SIZE = 30;

	var o:haxe.io.Output;
	var files:List<{
		name:String,
		compressed:Bool,
		clen:Int,
		size:Int,
		crc:Int,
		date:Date,
		fields:haxe.io.Bytes
	}>;

	public function new(o:haxe.io.Output) {
		this.o = o;
		files = new List();
	}

	function writeZipDate(date:Date) {
		var hour = date.getHours();
		var min = date.getMinutes();
		var sec = date.getSeconds() >> 1;
		o.writeUInt16((hour << 11) | (min << 5) | sec);
		var year = date.getFullYear() - 1980;
		var month = date.getMonth() + 1;
		var day = date.getDate();
		o.writeUInt16((year << 9) | (month << 5) | day);
	}

	public function writeEntryHeader(f:Entry) {
		var o = this.o;
		var flags = 0;
		if (f.extraFields != null) {
			for (e in f.extraFields)
				switch (e) {
					case FUtf8:
						flags |= 0x800;
					default:
				}
		}
		o.writeInt32(0x04034B50);
		o.writeUInt16(0x0014); // version
		o.writeUInt16(flags); // flags
		if (f.data == null) {
			f.fileSize = 0;
			f.dataSize = 0;
			f.crc32 = 0;
			f.compressed = false;
			f.data = haxe.io.Bytes.alloc(0);
		} else {
			if (f.crc32 == null) {
				if (f.compressed)
					throw "CRC32 must be processed before compression";
				f.crc32 = haxe.crypto.Crc32.make(f.data);
			}
			if 
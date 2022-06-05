
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

package sys.io;

import php.*;
import haxe.io.*;
import haxe.SysTools;

using StringTools;
using php.Global;

@:forward(iterator)
private abstract ProcessPipes(NativeIndexedArray<Resource>) from NativeIndexedArray<Resource> to NativeIndexedArray<Resource> {
	public var stdin(get, never):Resource;
	public var stdout(get, never):Resource;
	public var stderr(get, never):Resource;

	inline function get_stdin()
		return this[0];

	inline function get_stdout()
		return this[1];

	inline function get_stderr()
		return this[2];
}

private class ReadablePipe extends Input {
	var pipe:Resource;
	var tmpBytes:Bytes;

	public function new(pipe:Resource) {
		this.pipe = pipe;
		tmpBytes = Bytes.alloc(1);
	}

	override public function close():Void {
		pipe.fclose();
	}

	override public function readByte():Int {
		if (readBytes(tmpBytes, 0, 1) == 0)
			throw Error.Blocked;
		return tmpBytes.get(0);
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		if (pipe.feof())
			throw new Eof();

		var result = pipe.fread(len);
		if (result == "")
			throw new Eof();
		if (result == false)
			return throw Error.Custom('Failed to read process output');
		var result:String = result;

		var bytes = Bytes.ofString(result);
		s.blit(pos, bytes, 0, result.strlen());
		return result.strlen();
	}
}

private class WritablePipe extends Output {
	var pipe:Resource;
	var tmpBytes:Bytes;

	public function new(pipe:Resource) {
		this.pipe = pipe;
		tmpBytes = Bytes.alloc(1);
	}

	override public function close():Void {
		pipe.fclose();
	}

	override public function writeByte(c:Int):Void {
		tmpBytes.set(0, c);
		writeBytes(tmpBytes, 0, 1);
	}

	override public function writeBytes(b:Bytes, pos:Int, l:Int):Int {
		var s = b.getString(pos, l);
		if (pipe.feof())
			throw new Eof();

		var result = Global.fwrite(pipe, s, l);
		if (result == false)
			throw Error.Custom('Failed to write to process input');
		return result;
	}
}

class Process {
	public var stdout(default, null):Input;

	public var stderr(default, null):Input;

	public var stdin(default, null):Output;

	var process:Resource;
	var pipes:ProcessPipes;
	var pid:Int = -1;
	var running:Bool = true;
	var _exitCode:Int = -1;
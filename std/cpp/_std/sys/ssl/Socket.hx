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

package sys.ssl;

import cpp.NativeSocket;
import cpp.NativeSsl;

private typedef SocketHandle = Dynamic;
private typedef CONF = Dynamic;
private typedef SSL = Dynamic;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s:Socket;

	public function new(s:Socket) {
		this.__s = s;
	}

	public override function readByte() {
		return try {
			__s.handshake();
			NativeSsl.ssl_recv_char(@:privateAccess __s.ssl);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw haxe.io.Error.Blocked;
			else if (__s == null)
				throw haxe.io.Error.Custom(e);
			else
				throw new haxe.io.Eof();
		}
	}

	public override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		var r:Int;
		if (__s == null)
			throw "Invalid handle";
		try {
			__s.handshake();
			r = NativeSsl.ssl_recv(@:privateAccess __s.ssl, buf.getData(), pos, len);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
		if (r == 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}
}

private class SocketOutput extends haxe
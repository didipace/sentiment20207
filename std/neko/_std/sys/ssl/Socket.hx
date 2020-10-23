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

private typedef SocketHandle = Dynamic;
private typedef CTX = Dynamic;
private typedef SSL = Dynamic;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s:Socket;

	public function new(s:Socket) {
		this.__s = s;
	}

	public override function readByte() {
		return try {
			__s.handshake();
			ssl_recv_char(@:privateAccess __s.ssl);
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
			r = ssl_recv(@:privateAccess __s.ssl, buf.getData(), pos, len);
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

	private static var ssl_recv = neko.Lib.loadLazy("ssl", "ssl_recv", 4);
	private static var ssl_recv_char = neko.Lib.loadLazy("ssl", "ssl_recv_char", 1);
}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var __s:Socket;

	public function new(s:Socket) {
		this.__s = s;
	}

	public override function writeByte(c:Int) {
		if (__s == null)
			throw "Invalid handle";
		try {
			__s.handshake();
			ssl_send_char(@:privateAccess __s.ssl, c);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		return try {
			__s.handshake();
			ssl_send(@:privateAccess __s.ssl, buf.getData(), pos, len);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}

	private static var ssl_send_char = neko.Lib.loadLazy("ssl", "ssl_send_char", 2);
	private static var ssl_send = neko.Lib.loadLazy("ssl", "ssl_send", 4);
}

@:coreApi
class Socket extends sys.net.Socket {
	public static var DEFAULT_VERIFY_CERT:Null<Bool> = true;

	public static var DEFAULT_CA:Null<Certificate>;

	private var ctx:CTX;
	private var ssl:SSL;

	public var verifyCert:Null<Bool>;

	private var caCert:Null<Certificate>;
	private var hostname:String;

	private var ownCert:Null<Certificate>;
	private var ownKey:Null<Key>;
	private var altSNIContexts:Null<Array<{match:String->Bool, key:Key, cert:Certificate}>>;
	private var sniCallback:Dynamic;
	private var handshakeDone:Bool;

	private override function init():Void {
		__s = socket_new(false);
		input = new SocketInput(this);
		output = new SocketOutput(this);
		if (DEFAULT_VERIFY_CERT && DEFAULT_CA == null) {
			try {
				DEFAULT_CA = Certificate.loadDefaults();
			} catch (e:Dynamic) {}
		}
		verifyCert = DEFAULT_VERIFY_CERT;
		caCert = DEFAULT_CA;
	}

	public override function connect(host:sys.net.Host, port:Int):Void {
		try {
			ctx = buildSSLContext(fal
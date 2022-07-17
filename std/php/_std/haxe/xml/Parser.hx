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

package haxe.xml;

import php.Global;
import php.Syntax;
import php.NativeString;

using haxe.xml.Parser;

private enum abstract S(Int) {
	var IGNORE_SPACES;
	var BEGIN;
	var BEGIN_NODE;
	var TAG_NAME;
	var BODY;
	var ATTRIB_NAME;
	var EQUALS;
	var ATTVAL_BEGIN;
	var ATTRIB_VAL;
	var CHILDS;
	var CLOSE;
	var WAIT_END;
	var WAIT_END_RET;
	var PCDATA;
	var HEADER;
	var COMMENT;
	var DOCTYPE;
	var CDATA;
	var ESCAPE;
}

class XmlParserException {
	public var message:String;

	public var lineNumber:Int;

	public var positionAtLine:Int;

	public var position:Int;

	public var xml:String;

	public function new(message:String, xml:String, position:Int) {
		this.xml = xml;
		this.message = message;
		this.position = position;
		lineNumber = 1;
		positionAtLine = 0;

		for (i in 0...position) {
			var c = (xml : NativeString).fastCodeAt(i);
			if (c == '\n'.code) {
				lineNumber++;
				positionAtLine = 0;
			} else {
				if (c != '\r'.code)
					positionAtLine++;
			}
		}
	}

	public function toString():String {
		return Type.getClassName(Type.getClass(this)) + ": " + message + " at line " + lineNumber + " char " + positionAtLine;
	}
}

class Parser {
	static var escapes = {
		var h = new haxe.ds.StringMap();
		h.set("lt", "<");
		h.set("gt", ">");
		h.set("amp", "&");
		h.set("quot", '"');
		h.set("apos", "'");
		h;
	}

	static public function parse(str:String, strict = false) {
		var doc = Xml.createDocument(
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
		var doc = Xml.createDocument();
		doParse(str, strict, 0, doc);
		return doc;
	}

	static function doParse(str:NativeString, strict:Bool, p:Int = 0, ?parent:Xml):Int {
		var xml:Xml = null;
		var state = S.BEGIN;
		var next = S.BEGIN;
		var aname = null;
		var start = 0;
		var nsubs = 0;
		var nbrackets = 0;
		var c = str.fastCodeAt(p);
		var buf:NativeString = '';
		// need extra state because next is in use
		var escapeNext = S.BEGIN;
		var attrValQuote = -1;
		inline function addChild(xml:Xml) {
			parent.addChild(xml);
			nsubs++;
		}
		while (!StringTools.isEof(c)) {
			switch (state) {
				case S.IGNORE_SPACES:
					switch (c) {
						case '\n'.code, '\r'.code, '\t'.code, ' '.code:
						default:
							state = next;
							continue;
					}
				case S.BEGIN:
					switch (c) {
						case '<'.code:
							state = S.IGNORE_SPACES;
							next = S.BEGIN_NODE;
						default:
							start = p;
							state = S.PCDATA;
							continue;
					}
				case S.PCDATA:
					if (c == '<'.code) {
						buf = buf.addSub(str, start, p - start);
						var child = Xml.createPCData(buf);
						buf = '';
						addChild(child);
						state = S.IGNORE_SPACES;
						next = S.BEGIN_NODE;
					} else if (c == '&'.code) {
						buf = buf.addSub(str, start, p - start);
						state = S.ESCAPE;
						escapeNext = S.PCDATA;
						start = p + 1;
					}
				case S.CDATA:
					if (c == ']'.code && str.fastCodeAt(p + 1) == ']'.code && str.fastCodeAt(p + 2) == '>'.code) {
						var child = Xml.createCData(str.substr(start, p - start));
						addChild(child);
						p += 2;
						state = S.BEGIN;
					}
				case S.BEGIN_NODE:
					switch (c) {
						case '!'.code:
							if (str.fastCodeAt(p + 1) == '['.code) {
								p += 2;
								if (Global.strtoupper(str.substr(p, 6)) != "CDATA[")
									throw new XmlParserException("Expected <![CDATA[", str, p);
								p += 5;
								state = S.CDATA;
								start = p + 1;
							} else if (str.fastCodeAt(p + 1) == 'D'.code || str.fastCodeAt(p + 1) == 'd'.code) {
								if (Global.strtoupper(str.substr(p + 2, 6)) != "OCTYPE")
									throw new XmlParserException("Expected <!DOCTYPE", str, p);
								p += 8;
								state = S.DOCTYPE;
								start = p + 1;
							} else if (str.fastCodeAt(p + 1) != '-'.code || str.fastCodeAt(p + 2) != '-'.code)
								throw new XmlParserException("Expected <!--", str, p);
							else {
								p += 2;
								state = S.COMMENT;
								start = p + 1;
							}
						case '?'.code:
							state = S.HEADER;
							start = p;
						case '/'.code:
							if (parent == null)
								throw new XmlParserException("Expected node name", str, p);
							start = p + 1;
							state = S.IGNORE_SPACES;
							next = S.CLOSE;
						default:
							state = S.TAG_NAME;
							start = p;
							continue;
					}
				case S.TAG_NAME:
					if (!isValidChar(c)) {
						if (p == start)
							throw new XmlParserException("Expected node name", str, p);
						xml = Xml.createElement(str.substr(start, p - start));
						addChild(xml);
						state = S.IGNORE_SPACES;
						next = S.BODY;
						continue;
					}
				case S.BODY:
					switch (c) {
						case '/'.code:
							state = S.WAIT_END;
						case 
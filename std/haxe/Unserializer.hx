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

using haxe.Unserializer;

import haxe.ds.List;

@:noDoc
typedef TypeResolver = {
	function resolveClass(name:String):Class<Dynamic>;
	function resolveEnum(name:String):Enum<Dynamic>;
}

/**
	The `Unserializer` class is the complement to the `Serializer` class. It parses
	a serialization `String` and creates objects from the contained data.

	This class can be used in two ways:

	- create a `new Unserializer()` instance with a given serialization
		String, then call its `unserialize()` method until all values are
		extracted
	- call `Unserializer.run()`  to unserialize a single value from a given
		String

	The specification of the serialization format can be found here:
	<https://haxe.org/manual/serialization/format>
**/
class Unserializer {
	/**
		This value can be set to use custom type resolvers.

		A type resolver finds a `Class` or `Enum` instance from a given `String`.
		By default, the Haxe `Type` Api is used.

		A type resolver must provide two methods:

		1. `resolveClass(name:String):Class<Dynamic>` is called to determine a
				`Class` from a class name
		2. `resolveEnum(name:String):Enum<Dynamic>` is called to determine an
				`Enum` from an enum name

		This value is applied when a new `Unserializer` instance is created.
		Changing it afterwards has no effect on previously created instances.
	**/
	public static var DEFAULT_RESOLVER:TypeResolver = new DefaultResolver();

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789%:";

	#if !neko
	static var CODES = null;

	static function initCodes() {
		var codes = #if flash new flash.utils.ByteArray(); #else new Array(); #end
		for (i in 0...BASE64.length)
			codes[StringTools.fastCodeAt(BASE64, i)] = i;
		return codes;
	}
	#end

	var buf:String;
	var pos:Int;
	var length:Int;
	var cache:Array<Dynamic>;
	var scache:Array<String>;
	var resolver:TypeResolver;
	#if neko
	var upos:Int;
	#end

	/**
		Creates a new Unserializer instance, with its internal buffer
		initialized to `buf`.

		This does not parse `buf` immediately. It is parsed only when calls to
		`this.unserialize` are made.

		Each Unserializer instance maintains its own cache.
	**/
	public function new(buf:String) {
		this.buf = buf;
		length = this.buf.fastLength();
		pos = 0;
		#if neko
		upos = 0;
		#end
		scache = new Array();
		cache = new Array();
		var r = DEFAULT_RESOLVER;
		if (r == null) {
			r = new DefaultResolver();
			DEFAULT_RESOLVER = r;
		}
		resolver = r;
	}

	/**
		Sets the type resolver of `this` Unserializer instance to `r`.

		If `r` is `null`, a special resolver is used which returns `null` for all
		input values.

		See `DEFAULT_RESOLVER` for more information on type resolvers.
	**/
	public function setResolver(r) {
		if (r == null)
			resolver = NullResolver.instance;
		else
			resolver = r;
	}

	/**
		Gets the type resolver of `this` Unserializer instance.

		See `DEFAULT_RESOLVER` for more information on type resolvers.
	**/
	public function getResolver() {
		return resolver;
	}

	inline function get(p:Int):Int {
		#if php
		return p >= length ? 0 : buf.fastCharCodeAt(p);
		#else
		return StringTools.fastCodeAt(buf, p);
		#end
	}

	function readDigits() {
		var k = 0;
		var s = false;
		var fpos = pos;
		while (true) {
			var c = get(pos);
			if (StringTools.isEof(c))
				break;
			if (c == "-".code) {
				if (pos != fpos)
					break;
				s = true;
				pos++;
				continue;
			}
			if (c < "0".code || c > "9".code)
				break;
			k = k * 10 + (c - "0".code);
			pos++;
		}
		if (s)
			k *= -1;
		return k;
	}

	function readFloat() {
		var p1 = pos;
		while (true) {
			var c = get(pos);
			if (StringTools.isEof(c))
				break;
			// + - . , 0-9
			if ((c >= 43 && c < 58) || c == "e".code || c == "E".code)
				pos++;
			else
				break;
		}
		return Std.parseFloat(buf.fastSubstr(p1, pos - p1));
	}

	function unserializeObject(o:{}) {
		while (true) {
			if (pos >= length)
				throw "Invalid object";
			if (get(pos) == "g".code)
				break;
			var k:Dynamic = unserialize();
			if (!Std.isOfType(k, String))
				throw "Invalid object key";
			var v = unserialize();
			Reflect.setField(o, k, v);
		}
		pos++;
	}

	function unserializeEnum<T>(edecl:Enum<T>, tag:String) {
		if (get(pos++) != ":".code)
			throw "Invalid enum format";
		var nargs = readDigits();
		if (nargs == 0)
			return Type.createEnum(edecl, tag);
		var args = new Array();
		while (nargs-- > 0)
			args.push(unserialize());
		return Type.createEnum(edecl, tag, args);
	}

	/**
		Unserializes the next part of `this` Unserializer instance and returns
		the according value.

		This function may call `this.resolver.resolveClass` to determine a
		Class from a String, and `this.resolver.resolveEnum` to determine an
		Enum from a String.

		If `this` Unserializer instance contains no more or invalid data, an
		exception is thrown.

		This operation may fail on structurally valid data if a type cannot be
		resolved or if a field cannot be set. This can happen when unserializing
		Strings that were serialized on a different Haxe target, in which the
		serialization side has to make sure not to include platform-specific
		data.

		Classes are created from `Type.createEmptyInstance`, which means their
		constructors are not called.
	**/
	public function unserialize():Dynamic {
		switch (get(pos++)) {
			case "n".code:
				return null;
			case "t".code:
				return true;
			case "f".code:
				return false;
			case "z".code:
				return 0;
			case "i".code:
				return readDigits();
			case "d".code:
				return readFloat();
			case "y".code:
				var len = readDigits();
				if (get(pos++) != ":".code || length - pos < len)
					throw "Invalid string length";
				var s = buf.fastSubstr(pos, len);
				pos += len;
				s = StringTools.urlDecode(s);
				scache.push(s);
				return s;
			case "k".code:
				return Math.NaN;
			case "m".code:
				return Math.NEGATIVE_INFINITY;
			case "p".code:
				return Math.POSITIVE_INFINITY;
			case "a".code:
				var buf = buf;
				var a = new Array<Dynamic>();
				#if cpp
				var cachePos = cache.length;
				#end
				cache.push(a);
				while (true) {
					var c = get(pos);
					if (c == "h".code) {
						pos++;
						break;
					}
					if (c == "u".code) {
						pos++;
						var n = readDigits();
						a[a.length + n - 1] = null;
					} else
						a.push(unserialize());
				}
				#if cpp
				return cache[cachePos] = cpp.NativeArray.resolveVirtualArray(a);
				#else
				return a;
				#end
			case "o".code:
				var o = {};
				cache.push(o);
				unserializeObject(o);
				return o;
			case "r".code:
				var n = readDigits();
				if (n < 0 || n >= cache.length)
					throw "Invalid reference";
				return cache[n];
			case "R".code:
				var n = readDigits();
				if (n < 0 || n >= scache.length)
					throw "Invalid string reference";
				return scache[n];
			case "x".code:
				throw unserialize();
			case "c".code:
				var name = unserialize();
				var cl = resolver.resolveClass(name);
				if (cl == null)
					throw "Class not found " + name;
				var o = Type.createEmptyInstance(cl);
				cache.push(o);
				unserializeObject(o);
				return o;
			case "w".code:
				var name = unserialize();
				var edecl = resolver.resolveEnum(name);
				if (edecl == null)
					throw "Enum not found " + name;
				var e = unserializeEnum(edecl, unserialize());
				cache.push(e);
				return e;
			case "j".code:
				var name = unserialize();
				var edecl = resolver.resolveEnum(name);
				if (edecl == null)
					throw "Enum not found " + name;
				pos++; /* skip ':' */
				var index = readDigits();
				var tag = Type.getEnumConstructs(edecl)[index];
				if (tag == null)
					throw "Unknown enum index " + name + "@" + index;
				var e = unserializeEnum(edecl, tag);
			
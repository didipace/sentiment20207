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

	static var B
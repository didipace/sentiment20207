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

package js.lib;

import js.lib.intl.NumberFormat.NumberFormatOptions;

/**
	The `Uint8ClampedArray` typed array represents an array of 8-bit unsigned integers clamped
	to 0-255; if you specified a value that is out of the range of [0,255], 0 or 255 will be set instead;
	if you specify a non-integer, the nearest integer will be set. The contents are initialized to `0`.
	Once established, you can reference elements in the array using the object's methods, or using
	standard array index syntax (that is, using bracket notation).

	Documentation [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Uint8ClampedArray")
extern class Uint8ClampedArray implements ArrayBufferView implements ArrayAccess<Int> {
	/**
		Returns a number value of the element size. 1 in the case of an `Uint8ClampedArray`.
	 */
	static final BYTES_PER_ELEMENT:Int;

	/**
		Creates a new `Uint8ClampedArray` from an array-like or iterable object. See also [Array.from()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from).
	 */
	@:overload(function(source:{}, ?mapFn:(value:Int) -> Int, ?thisArg:Any):Uint8ClampedArray {})
	@:pure static function from(source:{}, ?mapFn:(value:Int, index:Int) -> Int, ?thisArg:Any):Uint8ClampedArray;

	/**
		Creates a new `Uint8ClampedArray` with a variable number of arguments. See also [Array.of()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of).
	 */
	@:pure static function of(elements:haxe.extern.Rest<Dynamic>):Uint8ClampedArray;

	/**
		Returns a number value of the element size.
	 */
	@:native("BYTES_PER_ELEMENT")
	final BYTES_PER_ELEMENT_:Int;

	/**
		Returns the `ArrayBuffer` referenced by the `Uint8ClampedArray` Fixed at construction time and thus read only.
	 */
	final buffer:ArrayBuffer;

	/**
		Returns the length (in bytes) of the `Uint8ClampedArray` from the start of its `ArrayBuffer`. Fixed at construction time and thus read only.
	 */
	final byteLength:Int;

	/**
		Returns the offset (in bytes) of the `Uint8ClampedArray` from the start of its `ArrayBuffer`. Fixed at construction time and thus read only.
	 */
	final byteOffset:Int;

	/**
		Returns the number of elements hold in the `Uint8ClampedArray`. Fixed at construction time and thus read only.
	 */
	final length:Int;

	/** @throws DOMError */
	@:overload(function(length:Int):Void {})
	@:overload(function(object:{}):Void {
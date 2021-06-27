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

package php;

/**
	This class contains externs for native PHP constants defined in global namespace.
	For native PHP functions in global namespace see `php.Global`.
**/
@:phpGlobal
extern class Const {
	/**
		If this constant is defined and equals `true` then Haxe will not set error handler automatically.
	**/
	static final HAXE_CUSTOM_ERROR_HANDLER:Bool;

	/**
		@see http://php.net/manual/en/reserved.constants.php
	**/
	static final PHP_VERSION_ID:Int;

	static final PHP_OS:String;
	static final PHP_SAPI:String;
	static final PHP_BINARY:String;
	static final PHP_EOL:String;
	static final PHP_INT_MAX:Int;
	static final PHP_INT_MIN:Int;
	static final PHP_INT_SIZE:Int;

	/**
		@see http://php.net/manual/en/language.constants.predefined.php
	**/
	static final __LINE__:Int;

	static final __FILE__:String;
	static final __DIR__:String;
	static final __FUNCTION__:String;
	static final __CLASS__:String;
	static final __TRAIT__:String;
	static final __METHOD__:String;
	static fin
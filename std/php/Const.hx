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
	static final __NAMESPACE__:String;

	/**
		@see https://php.net/manual/en/dir.constants.php
	**/
	static final DIRECTORY_SEPARATOR:String;

	static final PATH_SEPARATOR:String;
	static final SCANDIR_SORT_ASCENDING:Int;
	static final SCANDIR_SORT_DESCENDING:Int;
	static final SCANDIR_SORT_NONE:Int;

	/**
		@see http://php.net/manual/en/errorfunc.constants.php
	**/
	static final E_ERROR:Int;

	static final E_WARNING:Int;
	static final E_PARSE:Int;
	static final E_NOTICE:Int;
	static final E_CORE_ERROR:Int;
	static final E_CORE_WARNING:Int;
	static final E_COMPILE_ERROR:Int;
	static final E_COMPILE_WARNING:Int;
	static final E_USER_ERROR:Int;
	static final E_USER_WARNING:Int;
	static final E_USER_NOTICE:Int;
	static final E_STRICT:Int;
	static final E_RECOVERABLE_ERROR:Int;
	static final E_DEPRECATED:Int;
	static final E_USER_DEPRECATED:Int;
	static final E_ALL:Int;

	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static final COUNT_NORMAL:Int;

	static final COUNT_RECURSIVE:Int;

	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	static final ARRAY_FILTER_USE_KEY:Int;

	static final ARRAY_FILTER_USE_BOTH:Int;

	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static final DEBUG_BACKTRACE_PROVIDE_OBJECT:Int;

	static final DEBUG_BACKTRACE_IGNORE_ARGS:Int;

	/**
		@see http://php.net/manual/en/math.constants.php
	**/
	static final M_PI:Float;

	static final M_E:Float;
	static final M_LOG2E:Float;
	static final M_LOG10E:Float;
	static final M_LN2:Float;
	static final M_LN10:Float;
	static final M_PI_2:Float;
	static final M_PI_4:Float;
	static final M_1_PI:Float;
	static final M_2_PI:Float;
	static final M_SQRTPI:Float;
	static final M_2_SQRTPI:Float;
	static final M_SQRT2:Float;
	static final M_SQRT3:Float;
	static final M_SQRT1_2:Float;
	static final M_LNPI:Float;
	static final M_EULER:Float;
	static final PHP_ROUND_HALF_UP:Int;
	static final PHP_ROUND_HALF_DOWN:Int;
	static final PHP_ROUND_HALF_EVEN:Int;
	static final PHP_ROUND_HALF_ODD:Int;
	static final NAN:Float;
	static final INF:Float;

	/**
		@see http://php.net/manual/en/function.setlocale.php
	**/
	static final LC_ALL:Int;

	static final LC_COLLATE:Int;
	static final LC_CTYPE:Int;
	static final LC_MONETARY:Int;
	static final LC_NUMERIC:Int;
	static final LC_TIME:Int;
	static final LC_MESSAGES:Int;

	/**
		@see http://php.net/manual/en/features.commandline.io-streams.php
	**/
	static final STDIN:Resource;

	static final STDOUT:Resource;
	static final STDERR:Resource;

	/**
		@see http://php.net/manual/en/function.preg-match-all.php
	**/
	static final PREG_PATTERN_ORDER:Int;

	static final PREG_SET_ORDER:Int;
	static final PREG_OFFSET_CAPTURE:Int;

	/**
		@see http://php.net/manual/en/function.preg-split.php
	**/
	static final PREG_SPLIT_NO_EMPTY:Int;

	static final PREG_SPLIT_DELIM_CAPTURE:Int;
	static final PREG_SPLIT_OFFSET_CAPTURE:Int;

	/**
		@see http://php.net/manual/en/function.preg-last-error.php
	**/
	static final PREG_NO_ERROR:Int;

	static final PREG_INTERNAL_E
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

package lua;

import haxe.extern.Rest;
import haxe.Constraints.Function;

/**
	Externs for native Lua coroutines.
**/
@:native("_G.coroutine")
extern class Coroutine<T:Function> extends Thread {
	/**
		Creates a new coroutine, with body `f`. `f` must be a Lua function.
	**/
	static function create<T:Function>(f:T):Coroutine<T>;

	/**
		Returns the running coroutine plus a boolean, true when the running coroutine is the main one.
	**/
	static function running():CoroutineRunning;

	/**
		Returns the status of coroutine.
	**/
	static function status(c:Coroutine<Dynamic>):CoroutineState;

	/**
		Starts or continues the execution of coroutine.
		The first time you resume a coroutine, it starts running its body.
		The values `args` are passed as the arguments to the body function.
		If the coroutine has yielded, `resume` restarts it;
		the values
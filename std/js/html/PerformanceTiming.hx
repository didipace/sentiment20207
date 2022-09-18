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

// This file is generated from mozilla\PerformanceTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceTiming` interface is a legacy interface kept for backwards compatibility and contains properties that offer performance timing information for various events which occur during the loading and use of the current page. You get a `PerformanceTiming` object describing your page using the `window.performance.timing` property.

	Documentation [PerformanceTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming>
**/
@:deprecated("PerformanceTiming is deprecated, use the PerformanceNavigationTiming interface instead")
@:native("PerformanceTiming")
extern class PerformanceTiming {
	
	/**
		When the prompt for unload terminates on the previous document in the same browsing context. If there is no previous document, this value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var navigationStart(default,null) : Int;
	
	/**
		When the `unload` event has been thrown, indicating the time at which the previous document in the window began to unload. If there is no previous document, or if the previous document or one of the needed redirects is not of the same origin, the value returned is `0`.
	**/
	var unloadEventStart(default
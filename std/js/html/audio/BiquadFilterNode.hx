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

// This file is generated from mozilla\BiquadFilterNode.webidl. Do not edit!

package js.html.audio;

/**
	The `BiquadFilterNode` interface represents a simple low-order filter, and is created using the `AudioContext.createBiquadFilter()` method. It is an `AudioNode` that can represent different kinds of filters, tone control devices, and graphic equalizers.

	Documentation [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode>
**/
@:native("BiquadFilterNode")
extern class BiquadFilterNode extends AudioNode {
	
	/**
		Is a string value defining the kind of filtering algorithm the node is implementing.<br>
			 
			<table class="standard-table">
				The meaning of the different parameters depending of the type of the filter (detune has the same meaning regardless, so isn't listed below)
				
					<tr>
						<code>type</code>
						Description
						<code>frequency</code>
						<code>Q</code>
						<code>gain</code>
					</tr>
				
				
					<tr>
						<code>lowpass</code>
						<td>Standard second-order reson
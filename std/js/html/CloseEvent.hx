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

// This file is generated from mozilla\CloseEvent.webidl. Do not edit!

package js.html;

/**
	A `CloseEvent` is sent to clients using WebSockets when the connection is closed. This is delivered to the listener indicated by the `WebSocket` object's `onclose` attribute.

	Documentation [CloseEvent](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent>
**/
@:native("CloseEvent")
extern class CloseEvent extends Event {
	
	/**
		Returns a `Boolean` that Indicates whether or not the connection was cleanly closed.
	**/
	var wasClean(default,null) : Bool;
	
	/**
		Returns an <code>unsigned short</code> containing the close code send by the server. The following values are permitted status codes. The following definitions are sourced from the IANA website [Ref]. Note that the 1xxx codes are only WebSocket-internal and not for the same meaning by the transported data (like when the application-layer protocol is invalid). The only permitted codes to be specified in Firefox are 1000 and 3000 to 4999 [Source, Bug].
			<table id="Status_codes" class="standard-table">
				
					<tr>
						<td class="header">Status code</td>
						<td class="header">Name</td>
						<td class="header">Description</td>
					</tr>
					<tr>
						<td><code>0</code>–<code>999</code></td>
						<td> </td>
						<td>Reserved and not used.</td>
					</tr>
					<tr>
						<td><code>1000</code></td>
						<td>Normal Closure</td>
						<td>Normal closure; the connection successfully completed whatever purpose for which it was created.</td>
					</tr>
					<tr>
						<td><code>1001</code></td>
						<td>Going Away</td>
						<td>The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.</td>
					</tr>
					<tr>
	
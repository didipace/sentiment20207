
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

// This file is generated from mozilla\NotificationEvent.webidl. Do not edit!

package js.html;

/**
	The parameter passed into the `onnotificationclick` handler, the `NotificationEvent` interface represents a notification click event that is dispatched on the `ServiceWorkerGlobalScope` of a `ServiceWorker`.

	Documentation [NotificationEvent](https://developer.mozilla.org/en-US/docs/Web/API/NotificationEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/NotificationEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/NotificationEvent>
**/
@:native("NotificationEvent")
extern class NotificationEvent extends ExtendableEvent {
	
	/**
		Returns a `Notification` object representing the notification that was clicked to fire the event.
	**/
	var notification(default,null) : Notification;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : NotificationEventInit ) : Void;
}
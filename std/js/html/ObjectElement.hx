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

// This file is generated from mozilla\HTMLObjectElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLObjectElement` interface provides special properties and methods (beyond those on the `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of `object` element, representing external resources.

	Documentation [HTMLObjectElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement>
**/
@:native("HTMLObjectElement")
extern class ObjectElement extends Element {
	
	/**
		Is a `DOMString` that reflects the `data` HTML attribute, specifying the address of a resource's data.
	**/
	var data : String;
	
	/**
		Is a `DOMString` that reflects the `type` HTML attribute, specifying the MIME type of the resource.
	**/
	var type : String;
	
	/**
		Is a `Boolean` that reflects the `typeMustMatch` HTML attribute, indicating if the resource specified by `data` must only be played if it matches the `type` attribute.
	**/
	var typeMustMatch : Bool;
	
	/**
		Is a `DOMString` that reflects the `name` HTML attribute, specifying the name of the browsing context.
	**/
	var name : String;
	
	/**
		Is a `DOMString` that reflects the `usemap` HTML attribute, specifying a `map` element to use.
	**/
	var useMap : String;
	
	/**
		Retuns a `HTMLFormElement` representing the object element's form owner, or null if there isn't one.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a `DOMString` that reflects the `width` HTML attribute, specifying the displayed width of the resource in CSS pix
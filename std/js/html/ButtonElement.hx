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

// This file is generated from mozilla\HTMLButtonElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLButtonElement` interface provides properties and methods (beyond the `button` object interface it also has available to them by inheritance) for manipulating the layout and presentation of button elements.

	Documentation [HTMLButtonElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement>
**/
@:native("HTMLButtonElement")
extern class ButtonElement extends Element {
	
	/**
		Is a `Boolean` indicating whether or not the control should have input focus when the page loads, unless the user overrides it, for example by typing in a different control. Only one form-associated element in a document can have this attribute specified.
	**/
	var autofocus : Bool;
	
	/**
		Is a `Boolean` indicating whether or not the control is disabled, meaning that it does not accept any clicks.
	**/
	var disabled : Bool;
	
	/**
		Is a `HTMLFormElement` reflecting the form that this button is associated with. If the button is a descendant of a form element, then this attribute is the ID of that form element.
		
			If the button is not a descendant of a form element, then the attribute can be the ID of any form element in the same document it is related to, or the `null` value if none matches.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a `DOMString` reflecting the URI of a resource that processes information submitted by the button. If specified, this attribute overrides the `action` attribute of the `form` element that owns this element.
	**/
	var formAction : String;
	
	/**
		Is a `DOMString` reflecting the type of content that is used to submit the form to the server. If specified, this attribute overrides the `enctype` attribute of the `form` element that owns this element.
	**/
	var formEnctype : String;
	
	/**
		Is a `DOMString` reflecting the HTTP method that the browser uses to submit the form. If specified, this attribute override
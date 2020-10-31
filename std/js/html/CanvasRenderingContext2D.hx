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

// This file is generated from mozilla\CanvasRenderingContext2D.webidl. Do not edit!

package js.html;

/**
	To get an object of this interface, call `getContext()` on a `canvas element`, supplying "2d" as the argument:

	Documentation [CanvasRenderingContext2D](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D>
**/
@:native("CanvasRenderingContext2D")
extern class CanvasRenderingContext2D {
	var canvas(default,null) : CanvasElement;
	var globalAlpha : Float;
	var globalCompositeOperation : String;
	var strokeStyle : haxe.extern.EitherType<String,haxe.extern.EitherType<CanvasGradient,CanvasPattern>>;
	var fillStyle : haxe.extern.EitherType<String,haxe.extern.EitherType<CanvasGradient,CanvasPattern>>;
	var filter : String;
	var imageSmoothingEnabled : Bool;
	var lineWidth : Float;
	var lineCap : String;
	var lineJoin : String;
	var miterLimit : Float;
	var lineDashOffset : Float;
	var shadowOffsetX : Float;
	var shadowOffsetY : Float;
	var shadowBlur : Float;
	var shadowColor : String;
	var font : String;
	var textAlign : String;
	var textBaseline : String;
	
	/** @throws DOMError */
	@:overload( function( image : js.html.svg.ImageElement, dx : Float, dy : Float) : Void {} )
	@:overload( function( image : CanvasElement, dx : Float, dy : Float) : Void {} )
	@:overload( function( image : VideoElement, dx : Float, dy : Float) : Void {} )
	@:overload( function( image : ImageBitmap, dx : Float, dy : Float) : Void {} )
	@:overload( function( image : js.html.svg.ImageElement, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : CanvasElement, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : VideoElement, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : ImageBitmap, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : js.html.svg.ImageElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : CanvasElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : VideoElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : ImageBitmap, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float) : Void {} )
	@:overload( function( image : ImageElement, dx : Float, dy : Float ) : Void {} )
	@:overload( function( image : ImageElement, dx : Float, dy : Float, dw : Float, dh : Float ) : Void {} )
	function dr
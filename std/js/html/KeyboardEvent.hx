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

// This file is generated from mozilla\KeyboardEvent.webidl. Do not edit!

package js.html;

/**
	`KeyboardEvent` objects describe a user interaction with the keyboard; each event describes a single interaction between the user and a key (or combination of a key with modifier keys) on the keyboard.

	Documentation [KeyboardEvent](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent>
**/
@:native("KeyboardEvent")
extern class KeyboardEvent extends UIEvent {
	static inline var DOM_KEY_LOCATION_STANDARD : Int = 0;
	static inline var DOM_KEY_LOCATION_LEFT : Int = 1;
	static inline var DOM_KEY_LOCATION_RIGHT : Int = 2;
	static inline var DOM_KEY_LOCATION_NUMPAD : Int = 3;
	static inline var DOM_VK_CANCEL : Int = 3;
	static inline var DOM_VK_HELP : Int = 6;
	static inline var DOM_VK_BACK_SPACE : Int = 8;
	static inline var DOM_VK_TAB : Int = 9;
	static inline var DOM_VK_CLEAR : Int = 12;
	static inline var DOM_VK_RETURN : Int = 13;
	static inline var DOM_VK_SHIFT : Int = 16;
	static inline var DOM_VK_CONTROL : Int = 17;
	static inline var DOM_VK_ALT : Int = 18;
	static inline var DOM_VK_PAUSE : Int = 19;
	static inline var DOM_VK_CAPS_LOCK : Int = 20;
	static inline var DOM_VK_KANA : Int = 21;
	static inline var DOM_VK_HANGUL : Int = 21;
	static inline var DOM_VK_EISU : Int = 22;
	static inline var DOM_VK_JUNJA : Int = 23;
	static inline var DOM_VK_FINAL : Int = 24;
	static inline var DOM_VK_HANJA : Int = 25;
	static inline var DOM_VK_KANJI : Int = 25;
	static inline var DOM_VK_ESCAPE : Int = 27;
	static inline var DOM_VK_CONVERT : Int = 28;
	static inline var DOM_VK_NONCONVERT : Int = 29;
	static inline var DOM_VK_ACCEPT : Int = 30;
	static inline var DOM_VK_MODECHANGE : Int = 31;
	static inline var DOM_VK_SPACE : Int = 32;
	st
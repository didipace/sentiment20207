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
	static inline var DOM_VK_PAGE_UP : Int = 33;
	static inline var DOM_VK_PAGE_DOWN : Int = 34;
	static inline var DOM_VK_END : Int = 35;
	static inline var DOM_VK_HOME : Int = 36;
	static inline var DOM_VK_LEFT : Int = 37;
	static inline var DOM_VK_UP : Int = 38;
	static inline var DOM_VK_RIGHT : Int = 39;
	static inline var DOM_VK_DOWN : Int = 40;
	static inline var DOM_VK_SELECT : Int = 41;
	static inline var DOM_VK_PRINT : Int = 42;
	static inline var DOM_VK_EXECUTE : Int = 43;
	static inline var DOM_VK_PRINTSCREEN : Int = 44;
	static inline var DOM_VK_INSERT : Int = 45;
	static inline var DOM_VK_DELETE : Int = 46;
	static inline var DOM_VK_0 : Int = 48;
	static inline var DOM_VK_1 : Int = 49;
	static inline var DOM_VK_2 : Int = 50;
	static inline var DOM_VK_3 : Int = 51;
	static inline var DOM_VK_4 : Int = 52;
	static inline var DOM_VK_5 : Int = 53;
	static inline var DOM_VK_6 : Int = 54;
	static inline var DOM_VK_7 : Int = 55;
	static inline var DOM_VK_8 : Int = 56;
	static inline var DOM_VK_9 : Int = 57;
	static inline var DOM_VK_COLON : Int = 58;
	static inline var DOM_VK_SEMICOLON : Int = 59;
	static inline var DOM_VK_LESS_THAN : Int = 60;
	static inline var DOM_VK_EQUALS : Int = 61;
	static inline var DOM_VK_GREATER_THAN : Int = 62;
	static inline var DOM_VK_QUESTION_MARK : Int = 63;
	static inline var DOM_VK_AT : Int = 64;
	static inline var DOM_VK_A : Int = 65;
	static inline var DOM_VK_B : Int = 66;
	static inline var DOM_VK_C : Int = 67;
	static inline var DOM_VK_D : Int = 68;
	static inline var DOM_VK_E : Int = 69;
	static inline var DOM_VK_F : Int = 70;
	static inline var DOM_VK_G : Int = 71;
	static inline var DOM_VK_H : Int = 72;
	static inline var DOM_VK_I : Int = 73;
	static inline var DOM_VK_J : Int = 74;
	static inline var DOM_VK_K : Int = 75;
	static inline var DOM_VK_L : Int = 76;
	static inline var DOM_VK_M : Int = 77;
	static inline var DOM_VK_N : Int = 78;
	static inline var DOM_VK_O : Int = 79;
	static inline var DOM_VK_P : Int = 80;
	static inline var DOM_VK_Q : Int = 81;
	static inline var DOM_VK_R : Int = 82;
	static inline var DOM_VK_S : Int = 83;
	static inline var DOM_VK_T : Int = 84;
	static inline var DOM_VK_U : Int = 85;
	static inline var DOM_VK_V : Int = 86;
	static inline var DOM_VK_W : Int = 87;
	static inline var DOM_VK_X : Int = 88;
	static inline var DOM_VK_Y : Int = 89;
	static inline var DOM_VK_Z : Int = 90;
	static inline var DOM_VK_WIN : Int = 91;
	static inline var DOM_VK_CONTEXT_MENU : Int = 93;
	static inline var DOM_VK_SLEEP : Int = 95;
	static inline var DOM_VK_NUMPAD0 : Int = 96;
	static inline var DOM_VK_NUMPAD1 : Int = 97;
	static inline var DOM_VK_NUMPAD2 : Int = 98;
	static inline var DOM_VK_NUMPAD3 : Int = 99;
	static inline var DOM_VK_NUMPAD4 : Int = 100;
	static inline var DOM_VK_NUMPAD5 : Int = 101;
	static inline var DOM_VK_NUMPAD6 : Int = 102;
	static inline var DOM_VK_NUMPAD7 : Int = 103;
	static inline var DOM_VK_NUMPAD8 : Int = 104;
	static inline var DOM_VK_NUMPAD9 : Int = 105;
	static inline var DOM_VK_MULTIPLY : Int = 106;
	static inline var DOM_VK_ADD : Int = 107;
	static inline var DOM_VK_SEPARATOR : Int = 108;
	static inline var DOM_VK_SUBTRAC
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

import hl.Ref;

@:coreApi final class Date {
	private var t:Int;

	public function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void {
		t = date_new(year, month, day, hour, min, sec);
	}

	public function getTime():Float {
		return date_get_time(t);
	}

	public function getFullYear():Int {
		var v = 0;
		date_get_inf(t, v, null, null, null, null, null, null);
		return v;
	}

	public function getMonth():Int {
		var v = 0;
		date_get_inf(t, null, v, null, null, null, null, null);
		return v;
	}

	public function getDate():Int {
		var v = 0;
		date_get_inf(t, null, null, v, null, null, null, null);
		return v;
	}

	public function getHours():Int {
		var v = 0;
		date_get_inf(t, null, null, null, v, null, null, null);
		return v;
	}

	public function getMinutes():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, v, null, null);
		return v;
	}

	public function getSeconds():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, v, null);
		return v;
	}

	public function getDay():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, null, v);
		return v;
	}

	public function getUTCFullYear():Int {
		var v = 0;
		date_get_utc_inf(t, v, null, null, null, null, null, null);
		return v;
	}

	public function getUTCMonth():Int {
		var v = 0;
		date_get_utc_inf(t, null, v, null, null, null, null, null);
		return v;
	}

	public function getUTCDate():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, v, null, null, null, null);
		return v;
	}

	public function getUTCHours():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, v, null, null, null);
		return v;
	}

	public function getUTCMinutes():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, v, null, null);
		return v;
	}

	public function getUTCSeconds():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, null, v, null);
		return v;
	}

	public function getUTCDay():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, null, null, v);
		return v;
	}

	public function getTimezoneOffset():Int {
		var y = 0;
		var mo = 0;
		var d = 0;
		var h = 0;
		var m = 0;
		var s = 0;
		date_get_utc_inf(t, y, mo, d, h, m, s, null);
		return Std.int((date_new(y, mo, d, h, m, s) - t) / 60);
	}

	@:keep public function toString():String {
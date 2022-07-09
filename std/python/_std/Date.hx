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

import python.lib.datetime.Datetime;
import python.lib.datetime.Timedelta;
import python.lib.datetime.Timezone;
import python.Syntax;

@:coreApi class Date {
	private var date:Datetime;
	private var dateUTC:Datetime;

	public function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void {
		if (year < Datetime.min.year)
			year = Datetime.min.year;
		if (day == 0)
			day = 1;
		date = makeLocal(new Datetime(year, month + 1, day, hour, min, sec, 0));
		dateUTC = date.astimezone(Timezone.utc);
	}

	public inline function getTime():Float {
		return date.timestamp() * 1000;
	}

	public inline function getHours():Int {
		return date.hour;
	}

	public inline function getMinutes():Int {
		return date.minute;
	}

	public inline function getSeconds():Int {
		return date.second;
	}

	public inline function getFullYear():Int {
		return date.year;
	}

	public inline function getMonth():Int {
		return date.month - 1;
	}

	public inline function getDate():Int {
		return date.day;
	}

	public inline function getDay():Int {
		return date.isoweekday() % 7;
	}

	public inline function getUTCHours():Int {
		return dateUTC.hour;
	}

	public inline function getUTCMinutes():Int {
		return dateUTC.minute;
	}

	public inline function getUTCSeconds():Int {
		return dateUTC.second;
	}

	public inline function getUTCFullYear():Int {
		return dateUTC.year;
	}

	public inline function getUTCMonth():Int {
		return dateUTC.month - 1;
	}

	public inline function getUTCDate():Int {
		return dateUTC.day;
	}

	public inline function getUTCDay():Int {
		return dateUTC.isoweekday() % 7;
	}

	public function getTimezoneOffset():Int {
		return -Std.int(Syntax.binop(date.utcoffset(), "/", new Timedelta(0, 60)));
	}

	public function toString():String {
		return date.strftime("%Y-%m-%d %H:%M:%S");
	}

	static public function now():Date {
		var d = new Date(2000, 0, 1, 0, 0, 0);
		d.date = makeLocal(Datetime.now());
		d.dateUTC = d.date.astimezone(Timezone.utc);
		return d;
	}

	static public function fromTime(t:Float):Date {
		var d = new Date(2000, 0, 1, 0, 0, 0);
		d.date = makeLocal(Datetime.fromtimestamp(t / 1000.0));
		d.dateUTC = d.date.astimezone(Timezone.utc);
		return d;
	}

	static function makeLocal(date:Datetime):Datetime {
		try {
			return date.astimezone();
		} catch (e:Dynamic) {
			// No way in vanilla Python <=3.5 to get the local timezone
			// Additionally dates close to the epoch <86400 will throw on astimezone
			var tzinfo = Datetime.now(Timezone.utc).astimezone().tzinfo;
			return
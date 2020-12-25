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

/**
	The DateTools class contains some extra functionalities for handling `Date`
	instances and timestamps.

	In the context of Haxe dates, a timestamp is defined as the number of
	milliseconds elapsed since 1st January 1970.
**/
class DateTools {
	#if php
	#elseif (neko && !(macro || interp))
	static var date_format = neko.Lib.load("std", "date_format", 2);
	#else
	static var DAY_SHORT_NAMES = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
	static var DAY_NAMES = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
	static var MONTH_SHORT_NAMES = [
		"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	];
	static var MONTH_NAMES = [
		"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
	];

	private static function __format_get(d:Date, e:String):String {
		return switch (e) {
			case "%":
				"%";
			case "a":
				DAY_SHORT_NAMES[d.getDay()];
			case "A":
				DAY_NAMES[d.getDay()];
			case "b", "h":
				MONTH_SHORT_NAMES[d.getMonth()];
			case "B":
				MONTH_NAMES[d.getMonth()];
			case "C":
				StringTools.lpad(Std.string(Std.int(d.getFullYear() / 100)), "0", 2);
			case "d":
				StringTools.lpad(Std.string(d.getDate()), "0", 2);
			case "D":
				__format(d, "%m/%d/%y");
			case "e":
				Std.string(d.getDate());
			case "F":
				__format(d, "%Y-%m-%d");
			case "H",
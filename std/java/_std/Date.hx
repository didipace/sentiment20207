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

package;

import haxe.Int64;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

@:coreApi class Date {
	private var date:Calendar;
	private var dateUTC:Calendar;

	public function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void {
		date = new GregorianCalendar(year, month, day, hour, min, sec);
		dateUTC = new GregorianCalendar(TimeZone.getTimeZone("UTC"));
		dateUTC.setTimeInMillis(date.getTimeInMillis());
	}

	public inline function getTime():Float {
		return cast date.getTimeInMillis();
	}

	public inline function getHours():Int {
		return date.get(Calendar.HOUR_OF_DAY);
	}

	public inline function getMinutes():Int {
		return date.get(Calendar.MINUTE);
	}

	public inline function getSeconds():Int {
		return date.get(Calendar.SECOND);
	}

	public inline function getFullYear():Int {
		return date.get(Calendar.YEAR);
	}

	public inline function getMonth():Int {
		return date.get(Calendar.MONTH);
	}

	public inline function getDate():Int {
		return date.get(Calendar.DAY_OF_MONTH);
	}

	public inline function getDay():Int {
		// SUNDAY in Java == 1, MONDAY == 2, ...
		return cast date.get(Calendar.DAY_OF_WEEK) - 1;
	}

	public inline function getUTCHours():Int {
		return dateUTC.get(Calendar.HOUR_OF_DAY);
	}

	public inline function getUTCMinutes():Int {
		retur
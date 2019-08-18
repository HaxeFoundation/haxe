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
		return dateUTC.get(Calendar.MINUTE);
	}

	public inline function getUTCSeconds():Int {
		return dateUTC.get(Calendar.SECOND);
	}

	public inline function getUTCFullYear():Int {
		return dateUTC.get(Calendar.YEAR);
	}

	public inline function getUTCMonth():Int {
		return dateUTC.get(Calendar.MONTH);
	}

	public inline function getUTCDate():Int {
		return dateUTC.get(Calendar.DAY_OF_MONTH);
	}

	public inline function getUTCDay():Int {
		// SUNDAY in Java == 1, MONDAY == 2, ...
		return cast dateUTC.get(Calendar.DAY_OF_WEEK) - 1;
	}

	public inline function getTimezoneOffset():Int {
		return -Std.int(date.get(Calendar.ZONE_OFFSET) / 60000);
	}

	public function toString():String {
		var m = getMonth() + 1;
		var d = getDate();
		var h = getHours();
		var mi = getMinutes();
		var s = getSeconds();
		return getFullYear() + "-" + (if (m < 10) "0" + m else "" + m) + "-" + (if (d < 10) "0" + d else "" + d) + " "
			+ (if (h < 10) "0" + h else "" + h) + ":" + (if (mi < 10) "0" + mi else "" + mi) + ":" + (if (s < 10) "0" + s else "" + s);
	}

	static public function now():Date {
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = Calendar.getInstance();
		d.dateUTC.setTimeInMillis(d.date.getTimeInMillis());
		return d;
	}

	static public function fromTime(t:Float):Date {
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date.setTimeInMillis(cast t);
		d.dateUTC.setTimeInMillis(cast t);
		return d;
	}

	static public function fromString(s:String):Date {
		switch (s.length) {
			case 8: // hh:mm:ss
				var k = s.split(":");
				return Date.fromTime(Std.parseInt(k[0]) * 3600000. + Std.parseInt(k[1]) * 60000. + Std.parseInt(k[2]) * 1000.);
			case 10: // YYYY-MM-DD
				var k = s.split("-");
				return new Date(Std.parseInt(k[0]), Std.parseInt(k[1]) - 1, Std.parseInt(k[2]), 0, 0, 0);
			case 19: // YYYY-MM-DD hh:mm:ss
				var k = s.split(" ");
				var y = k[0].split("-");
				var t = k[1].split(":");
				return new Date(Std.parseInt(y[0]), Std.parseInt(y[1]) - 1, Std.parseInt(y[2]), Std.parseInt(t[0]), Std.parseInt(t[1]), Std.parseInt(t[2]));
			default:
				throw "Invalid date format : " + s;
		}
	}
}

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

/**
	C# implementation of the Date class using System.DateTime.
**/
@:coreApi class Date {
	private var dateLocal:cs.system.DateTime;
	private var dateUTC:cs.system.DateTime;

	// Unix epoch in ticks (100-nanosecond intervals since 0001-01-01)
	private static var epochTicks:haxe.Int64 = new cs.system.DateTime(1970, 1, 1, 0, 0, 0, cs.system.DateTimeKind.Utc).Ticks;

	public function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void {
		dateLocal = new cs.system.DateTime(year, month + 1, day, hour, min, sec, cs.system.DateTimeKind.Local);
		dateUTC = dateLocal.ToUniversalTime();
	}

	public function getTime():Float {
		// Use Ticks (Int64) for precise millisecond calculation
		// TicksPerMillisecond = 10,000 (100-nanosecond intervals per ms)
		var ticksSinceEpoch:haxe.Int64 = dateUTC.Ticks - epochTicks;
		var milliseconds:haxe.Int64 = ticksSinceEpoch / cs.system.TimeSpan.TicksPerMillisecond;
		return cast(milliseconds, Float);
	}

	public function getHours():Int {
		return dateLocal.Hour;
	}

	public function getMinutes():Int {
		return dateLocal.Minute;
	}

	public function getSeconds():Int {
		return dateLocal.Second;
	}

	public function getFullYear():Int {
		return dateLocal.Year;
	}

	public function getMonth():Int {
		return dateLocal.Month - 1; // Haxe months are 0-based
	}

	public function getDate():Int {
		return dateLocal.Day;
	}

	public function getDay():Int {
		// DayOfWeek: Sunday = 0, Monday = 1, etc.
		return cast(dateLocal.DayOfWeek, Int);
	}

	public function getUTCHours():Int {
		return dateUTC.Hour;
	}

	public function getUTCMinutes():Int {
		return dateUTC.Minute;
	}

	public function getUTCSeconds():Int {
		return dateUTC.Second;
	}

	public function getUTCFullYear():Int {
		return dateUTC.Year;
	}

	public function getUTCMonth():Int {
		return dateUTC.Month - 1; // Haxe months are 0-based
	}

	public function getUTCDate():Int {
		return dateUTC.Day;
	}

	public function getUTCDay():Int {
		return cast(dateUTC.DayOfWeek, Int);
	}

	public function getTimezoneOffset():Int {
		var utcOffset = cs.system.TimeZoneInfo.Local.GetUtcOffset(dateLocal);
		return -Std.int(utcOffset.TotalMinutes);
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
		var d = new Date(1970, 0, 1, 0, 0, 0);
		d.dateLocal = cs.system.DateTime.Now;
		d.dateUTC = d.dateLocal.ToUniversalTime();
		return d;
	}

	static public function fromTime(t:Float):Date {
		var d = new Date(1970, 0, 1, 0, 0, 0);
		// Convert milliseconds to ticks (Int64) for precision using epoch + AddTicks
		var ticksFromEpoch:haxe.Int64 = haxe.Int64.fromFloat(t) * cs.system.TimeSpan.TicksPerMillisecond;
		var epoch = new cs.system.DateTime(1970, 1, 1, 0, 0, 0, cs.system.DateTimeKind.Utc);
		d.dateUTC = epoch.AddTicks(ticksFromEpoch);
		d.dateLocal = d.dateUTC.ToLocalTime();
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

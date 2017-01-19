/*
 * Copyright (C)2005-2017 Haxe Foundation
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
@:coreApi class Date {
	var d : lua.Os.DateType;
	var t : lua.Time;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) {
		t =  lua.Os.time({
			year  : year,
			month : month+1,
			day   : day,
			hour  : hour,
			min   : min,
			sec   : sec
		});
		d =lua.Os.date("*t", t);
	};
	public function getTime()     : Float return cast t * 1000;
	public function getHours()    : Int return d.hour;
	public function getMinutes()  : Int return d.min;
	public function getSeconds()  : Int return d.sec;
	public function getFullYear() : Int return d.year;
	public function getMonth()    : Int return d.month-1;
	public function getDate()     : Int return d.day;
	public function getDay()      : Int return d.wday-1;

	public inline function toString() : String {
		return lua.Boot.dateStr(this);
	}

	public static inline function now() : Date {
		return fromTime(lua.Os.time()*1000);
	}

	public static inline function fromTime( t : Float ) : Date {
		var d : Dynamic = {}
		untyped {
			lua.Lua.setmetatable(d, untyped {__index : Date.prototype});
			d.t = t/1000;
			d.d = lua.Os.date("*t", Std.int(d.t));
		}
		return d;
	}

	public static inline function fromString( s : String ) : Date {
		return lua.Boot.strDate(s);
	}
}


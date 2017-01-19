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
import python.lib.datetime.Datetime;
import python.lib.datetime.Timedelta;
import python.Syntax;

@:coreApi class Date
{
	static var EPOCH_UTC = Datetime.fromtimestamp(0, python.lib.datetime.Timezone.utc);

	private var date:Datetime;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void
	{
		if (year < Datetime.min.year) year = Datetime.min.year;
		if (day == 0) day = 1;
		date = new Datetime(year, month+1, day, hour, min, sec, 0);
	}

	public inline function getTime() : Float
	{
		return python.lib.Time.mktime(date.timetuple()) * 1000;
	}

	public inline function getHours() : Int
	{
		return date.hour;
	}

	public inline function getMinutes() : Int
	{
		return date.minute;
	}

	public inline function getSeconds() : Int
	{
		return date.second;
	}

	public inline function getFullYear() : Int
	{
		return date.year;
	}

	public inline function getMonth() : Int
	{
		return date.month-1;
	}

	public inline function getDate() : Int
	{
		return date.day;
	}

	public inline function getDay() : Int
	{
		return date.isoweekday();
	}

	public function toString():String
	{
		inline function st (x) return Std.string(x);

		var m = getMonth()+1;
		var d = getDate();
		var h = getHours();
		var mi = getMinutes();
		var s = getSeconds();
		return st(getFullYear())
			+"-"+(if( m < 10 ) "0"+st(m) else ""+st(m))
			+"-"+(if( d < 10 ) "0"+st(d) else ""+st(d))
			+" "+(if( h < 10 ) "0"+st(h) else ""+st(h))
			+":"+(if( mi < 10 ) "0"+st(mi) else ""+st(mi))
			+":"+(if( s < 10 ) "0"+st(s) else ""+st(s));
	}

	static public function now() : Date
	{
		var d = new Date(1970, 0, 1, 0, 0, 0);
		d.date = Datetime.now();
		return d;
	}

	static public function fromTime( t : Float ) : Date
	{
		var d = new Date(1970, 0, 1, 0, 0, 0);
		d.date = Datetime.fromtimestamp(t / 1000.0);
		return d;
	}


	static function UTC( year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Float
	{
		var dt = new Datetime(year, month+1, day, hour, min, sec, 0, python.lib.datetime.Timezone.utc);
		return datetimeTimestamp(dt, EPOCH_UTC);
	}


	static function datetimeTimestamp(dt:Datetime, epoch:Datetime):Float {
		return (Syntax.binop(dt, "-", epoch) : Timedelta).total_seconds() * 1000;
	}

	static public function fromString( s : String ) : Date
	{
		switch( s.length )
		{
			case 8: // hh:mm:ss
				var k = s.split(":");
				var d : Date = new Date(0, 0, 0, Std.parseInt(k[0]), Std.parseInt(k[1]), Std.parseInt(k[2]));
				return d;
			case 10: // YYYY-MM-DD
				var k = s.split("-");
				return new Date(Std.parseInt(k[0]),Std.parseInt(k[1]) - 1,Std.parseInt(k[2]),0,0,0);
			case 19: // YYYY-MM-DD hh:mm:ss
				var k = s.split(" ");
				var y = k[0].split("-");
				var t = k[1].split(":");
				return new Date(Std.parseInt(y[0]),Std.parseInt(y[1]) - 1,Std.parseInt(y[2]),Std.parseInt(t[0]),Std.parseInt(t[1]),Std.parseInt(t[2]));
			default:
				throw "Invalid date format : " + s;
		}
	}
}
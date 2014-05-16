/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import cs.system.DateTime;
import cs.system.TimeSpan;
import haxe.Int64;

#if core_api_serialize
@:meta(System.Serializable)
#end
@:coreApi class Date
{
	private var date:DateTime;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void
	{
		if (day <= 0) day = 1;
		if (year <= 0) year = 1;
		date = new DateTime(year, month + 1, day, hour, min, sec);
	}

	public inline function getTime() : Float
	{
		return (cast(date.Ticks, Float) / cast(TimeSpan.TicksPerMillisecond, Float));
	}

	public inline function getHours() : Int
	{
		return date.Hour;
	}

	public inline function getMinutes() : Int
	{
		return date.Minute;
	}

	public inline function getSeconds() : Int
	{
		return date.Second;
	}

	public inline function getFullYear() : Int
	{
		return date.Year;
	}

	public inline function getMonth() : Int
	{
		return date.Month - 1;
	}

	public inline function getDate() : Int
	{
		return date.Day;
	}

	public inline function getDay() : Int
	{
		return cast(date.DayOfWeek, Int);
	}

	public function toString():String
	{
		var m = getMonth() + 1;
		var d = getDate();
		var h = getHours();
		var mi = getMinutes();
		var s = getSeconds();
		return (getFullYear())
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	static public function now() : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = DateTime.Now;
		return d;
	}

	static public function fromTime( t : Float ) : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new DateTime(cast(t * cast(TimeSpan.TicksPerMillisecond, Float), Int64));
		return d;
	}

	static public function fromString( s : String ) : Date
	{
		switch( s.length )
		{
			case 8: // hh:mm:ss
				var k = s.split(":");
				var d : Date = new Date(1, 1, 1, Std.parseInt(k[0]), Std.parseInt(k[1]), Std.parseInt(k[2]));
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

	private static function fromNative( d : cs.system.DateTime ) : Date
	{
		var date = new Date(0, 0, 0, 0, 0, 0);
		date.date = d;
		return date;
	}
}

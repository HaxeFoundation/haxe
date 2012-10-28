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
import haxe.Int64;

@:SuppressWarnings("deprecation")
@:coreApi class Date
{
	private var date:java.util.Date;

	/**
		Creates a new date object.
	**/
	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void
	{
		date = new java.util.Date(year != 0 ? year - 1900 : 0, month, day, hour, min, sec);
	}

	/**
		Returns the timestamp of the date. It's the number of milliseconds
		elapsed since 1st January 1970. It might only have a per-second precision
		depending on the platforms.
	**/
	public inline function getTime() : Float
	{
		return cast date.getTime();
	}

	/**
		Returns the hours value of the date (0-23 range).
	**/
	public inline function getHours() : Int
	{
		return date.getHours();
	}

	/**
		Returns the minutes value of the date (0-59 range).
	**/
	public inline function getMinutes() : Int
	{
		return date.getMinutes();
	}

	/**
		Returns the seconds of the date (0-59 range).
	**/
	public inline function getSeconds() : Int
	{
		return date.getSeconds();
	}

	/**
		Returns the full year of the date.
	**/
	public inline function getFullYear() : Int
	{
		return date.getYear() + 1900;
	}

	/**
		Returns the month of the date (0-11 range).
	**/
	public inline function getMonth() : Int
	{
		return date.getMonth();
	}

	/**
		Returns the day of the date (1-31 range).
	**/
	public inline function getDate() : Int
	{
		return date.getDate();
	}

	/**
		Returns the week day of the date (0-6 range).
	**/
	public inline function getDay() : Int
	{
		return date.getDay();
	}

	/**
		Returns a string representation for the Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See [DateTools.format] for
		other formating rules.
	**/
	public function toString():String
	{
		var m = date.getMonth() + 1;
		var d = date.getDate();
		var h = date.getHours();
		var mi = date.getMinutes();
		var s = date.getSeconds();
		return (date.getYear() + 1900)
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	/**
		Returns a Date representing the current local time.
	**/
	static public function now() : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new java.util.Date();
		return d;
	}

	/**
		Returns a Date from a timestamp [t] which is the number of
		milliseconds elapsed since 1st January 1970.
	**/
	static public function fromTime( t : Float ) : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new java.util.Date(cast(t, Int64));
		return d;
	}

	/**
		Returns a Date from a formated string of one of the following formats :
		[YYYY-MM-DD hh:mm:ss] or [YYYY-MM-DD] or [hh:mm:ss]. The first two formats
		are expressed in local time, the third in UTC Epoch.
	**/
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
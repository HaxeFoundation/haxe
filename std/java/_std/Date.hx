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
package;
import haxe.Int64;

@:SuppressWarnings("deprecation")
@:coreApi class Date
{
	private var date:java.util.Date;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void
	{
		//issue #1769
		year = year != 0 ? year - 1900 : 0;
		date = new java.util.Date(year, month, day, hour, min, sec);
	}

	public inline function getTime() : Float
	{
		return cast date.getTime();
	}

	public inline function getHours() : Int
	{
		return date.getHours();
	}

	public inline function getMinutes() : Int
	{
		return date.getMinutes();
	}

	public inline function getSeconds() : Int
	{
		return date.getSeconds();
	}

	public inline function getFullYear() : Int
	{
		return date.getYear() + 1900;
	}

	public inline function getMonth() : Int
	{
		return date.getMonth();
	}

	public inline function getDate() : Int
	{
		return date.getDate();
	}

	public inline function getDay() : Int
	{
		return date.getDay();
	}

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

	static public function now() : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new java.util.Date();
		return d;
	}

	static public function fromTime( t : Float ) : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new java.util.Date(cast(t, Int64));
		return d;
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

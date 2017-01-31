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
@:coreApi  class Date {

	private var mSeconds:Float;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void	{
		mSeconds = untyped __global__.__hxcpp_new_date(year,month,day,hour,min,sec);
	}

	public function getTime() : Float {
		return mSeconds * 1000.0;
	}

	public function getHours() : Int { return untyped __global__.__hxcpp_get_hours(mSeconds); }

	public function getMinutes() : Int { return untyped __global__.__hxcpp_get_minutes(mSeconds); }

	public function getSeconds() : Int { return untyped __global__.__hxcpp_get_seconds(mSeconds); }

	public function getFullYear() : Int { return untyped __global__.__hxcpp_get_year(mSeconds); }

	public function getMonth() : Int { return untyped __global__.__hxcpp_get_month(mSeconds); }

	public function getDate() : Int { return untyped __global__.__hxcpp_get_date(mSeconds); }

	public function getDay() : Int { return untyped __global__.__hxcpp_get_day(mSeconds); }

	public function toString():String { return untyped __global__.__hxcpp_to_string(mSeconds); }

	public static function now() : Date {
		return fromTime( untyped __global__.__hxcpp_date_now()*1000.0);
	}
  	private static function new1(t : Dynamic) : Date {
		return  new Date(2005,1,1,0,0,0);
	}

	public static function fromTime( t : Float ) : Date {
		var result = new Date(0,0,0,0,0,0);
		result.mSeconds = t*0.001;
		return result;
	}

	public static function fromString( s : String ) : Date {
		switch( s.length ) {
			case 8: // hh:mm:ss
				var k = s.split(":");
				var d : Date = new Date(0,0,0,Std.parseInt(k[0]),Std.parseInt(k[1]),Std.parseInt(k[2]));
				return d;
			case 10: // YYYY-MM-DD
				var k = s.split("-");
				return new Date(Std.parseInt(k[0]),Std.parseInt(k[1])-1,Std.parseInt(k[2]),0,0,0);
			case 19: // YYYY-MM-DD hh:mm:ss
				var k = s.split(" ");
				var y = k[0].split("-");
				var t = k[1].split(":");
				return new Date(Std.parseInt(y[0]),Std.parseInt(y[1]) - 1,Std.parseInt(y[2]),
					Std.parseInt(t[0]),Std.parseInt(t[1]),Std.parseInt(t[2]));
			default:
				throw "Invalid date format : " + s;
		}
	}
}


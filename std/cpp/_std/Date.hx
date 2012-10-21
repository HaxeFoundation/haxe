/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
		return null;
	}
}


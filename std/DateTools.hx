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

/**
	The DateTools class contains some extra functionalities for handling `Date`
	instances and timestamps.

	In the context of Haxe dates, a timestamp is defined as the number of
	milliseconds elapsed since 1st January 1970.
**/
class DateTools {

	#if php
	#elseif (neko && !(macro || interp))
	static var date_format = neko.Lib.load("std","date_format",2);
	#else
	static var DAY_SHORT_NAMES = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
	static var DAY_NAMES = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
	static var MONTH_SHORT_NAMES = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
	static var MONTH_NAMES = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	
	private static function __format_get( d : Date, e : String ) : String {
		return switch( e ){
			case "%":
				"%";
			case "a":
				DAY_SHORT_NAMES[d.getDay()];
			case "A":
				DAY_NAMES[d.getDay()];
			case "b","h":
				MONTH_SHORT_NAMES[d.getMonth()];
			case "B":
				MONTH_NAMES[d.getMonth()];
			case "C":
				untyped StringTools.lpad(Std.string(Std.int(d.getFullYear()/100)),"0",2);
			case "d":
				untyped StringTools.lpad(Std.string(d.getDate()),"0",2);
			case "D":
				__format(d,"%m/%d/%y");
			case "e":
				untyped Std.string(d.getDate());
			case "F":
				__format(d,"%Y-%m-%d");
			case "H","k":
				untyped StringTools.lpad(Std.string(d.getHours()),if( e == "H" ) "0" else " ",2);
			case "I","l":
				var hour = d.getHours()%12;
				untyped StringTools.lpad(Std.string(hour == 0 ? 12 : hour),if( e == "I" ) "0" else " ",2);
			case "m":
				untyped StringTools.lpad(Std.string(d.getMonth()+1),"0",2);
			case "M":
				untyped StringTools.lpad(Std.string(d.getMinutes()),"0",2);
			case "n":
				"\n";
			case "p":
				untyped if( d.getHours() > 11 ) "PM"; else "AM";
			case "r":
				__format(d,"%I:%M:%S %p");
			case "R":
				__format(d,"%H:%M");
			case "s":
				Std.string(Std.int(d.getTime()/1000));
			case "S":
				untyped StringTools.lpad(Std.string(d.getSeconds()),"0",2);
			case "t":
				"\t";
			case "T":
				__format(d,"%H:%M:%S");
			case "u":
				untyped{
					var t = d.getDay();
					if( t == 0 ) "7"; else Std.string(t);
				}
			case "w":
				untyped Std.string(d.getDay());
			case "y":
				untyped StringTools.lpad(Std.string(d.getFullYear()%100),"0",2);
			case "Y":
				untyped Std.string(d.getFullYear());
			default:
				throw "Date.format %"+e+"- not implemented yet.";
		}
	}

	private static function __format( d : Date, f : String ) : String {
		var r = new StringBuf();
		var p = 0;
		while( true ){
			var np = f.indexOf("%", p);
			if( np < 0 )
				break;

			r.addSub(f,p,np-p);
			r.add( __format_get(d, f.substr(np+1,1) ) );

			p = np+2;
		}
		r.addSub(f,p,f.length-p);
		return r.toString();
	}
	#end

	/**
		Format the date `d` according to the format `f`. The format is
		compatible with the `strftime` standard format, except that there is no
		support in Flash and JS for day and months names (due to lack of proper
		internationalization API). On Haxe/Neko/Windows, some formats are not
		supported.

		```haxe
		var t = DateTools.format(Date.now(), "%Y-%m-%d_%H:%M:%S");
		// 2016-07-08_14:44:05

		var t = DateTools.format(Date.now(), "%r");
		// 02:44:05 PM

		var t = DateTools.format(Date.now(), "%T");
		// 14:44:05

		var t = DateTools.format(Date.now(), "%F");
		// 2016-07-08
		```
	**/
	public static function format( d : Date, f : String ) : String {
		#if (neko && !(macro || interp))
			return new String(untyped date_format(d.__t, f.__s));
		#elseif php
			return untyped __call__("strftime",f,d.__t);
		#else
			return __format(d,f);
		#end
	}

	/**
		Returns the result of adding timestamp `t` to Date `d`.

		This is a convenience function for calling
		`Date.fromTime(d.getTime() + t)`.
	**/
	public static inline function delta( d : Date, t : Float ) : Date {
		return Date.fromTime( d.getTime() + t );
	}

	static var DAYS_OF_MONTH = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

	/**
		Returns the number of days in the month of Date `d`.

		This method handles leap years.
	**/
	public static function getMonthDays( d : Date ) : Int {
		var month = d.getMonth();
		var year = d.getFullYear();

		if (month != 1)
			return DAYS_OF_MONTH[month];

		var isB = ((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0);
		return if (isB) 29 else 28;
	}

	/**
		Converts a number of seconds to a timestamp.
	**/
	public static inline function seconds( n : Float ) : Float {
		return n * 1000.0;
	}

	/**
		Converts a number of minutes to a timestamp.
	**/
	#if as3 @:extern #end public static inline function minutes( n : Float ) : Float {
		return n * 60.0 * 1000.0;
	}

	/**
		Converts a number of hours to a timestamp.
	**/
	public static inline function hours( n : Float ) : Float {
		return n * 60.0 * 60.0 * 1000.0;
	}

	/**
		Converts a number of days to a timestamp.
	**/
	public static inline function days( n : Float ) : Float {
		return n * 24.0 * 60.0 * 60.0 * 1000.0;
	}

	/**
		Separate a date-time into several components
	**/
	public static function parse( t : Float ) {
		var s = t / 1000;
		var m = s / 60;
		var h = m / 60;
		return {
			ms : t % 1000,
			seconds : Std.int(s % 60),
			minutes : Std.int(m % 60),
			hours : Std.int(h % 24),
			days : Std.int(h / 24),
		};
	}

	/**
		Build a date-time from several components
	**/
	public static function make( o : { ms : Float, seconds : Int, minutes : Int, hours : Int, days : Int } ) {
		return o.ms + 1000.0 * (o.seconds + 60.0 * (o.minutes + 60.0 * (o.hours + 24.0 * o.days)));
	}

	#if (js || flash || php || cpp || python)
	/**
		Retrieve Unix timestamp value from Date components. Takes same argument sequence as the Date constructor.
	**/
	public static #if (js || flash || php) inline #end function makeUtc(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ):Float {
	    #if (js || flash || python)
		   return untyped Date.UTC(year, month, day, hour, min, sec);
		#elseif php
		   return untyped __call__("gmmktime", hour, min, sec, month + 1, day, year) * 1000;
		#elseif cpp
		  return untyped __global__.__hxcpp_utc_date(year,month,day,hour,min,sec)*1000.0 ;
		#else
			//TODO
		   return 0.;
		#end
	}
	#end
}

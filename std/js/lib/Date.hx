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

package js.lib;

import Date in HaxeDate;

/**
	Creates a JavaScript Date instance that represents a single moment in time. Date objects are based on a time value that is the number of milliseconds since 1 January 1970 UTC.
**/
@:native("Date")
extern class Date {
	@:overload(function(value:Float):Void {})
	@:overload(function(dateString:String):Void {})
	@:overload(function(year:Int, month:Int, ?day:Int, ?hours:Int, ?minutes:Int, ?seconds:Int, ?milliseconds:Int):Void {})
	function new():Void;

	/**
		Cast Haxe's Date to js.lib.Date.
	**/
	static public inline function fromHaxeDate(date:HaxeDate):js.lib.Date {
		return cast date;
	}

	/**
		Cast js.lib.Date to Haxe's Date.
	**/
	static public inline function toHaxeDate(date:Date):HaxeDate {
		return cast date;
	}

	/**
		Returns the numeric value corresponding to the current time - the number of milliseconds elapsed since January 1, 1970 00:00:00 UTC, with leap seconds ignored
	**/
	static function now():Float;

	/**
		Parses a string representation of a date and returns the number of milliseconds since 1 January, 1970, 00:00:00, UTC, with leap seconds ignored.
	**/
	static function parse(str:String):Float;

	/**
		Returns the number of milliseconds since January 1, 1970, 00:00:00 UTC, with leap seconds ignored.
	**/
	static function UTC(year:Int, month:Int, ?day:Int, ?hours:Int, ?minutes:Int, ?seconds:Int, ?milliseconds:Int):Float;

	/**
		Returns the day of the month (1-31) for the specified date according to local time.
	**/
	function getDate():Int;

	/**
		Returns the day of the week (0-6) for the specified date according to local time.
	**/
	function getDay():Int;

	/**
		Returns the year (4 digits for 4-digit years) of the specified date according to local time.
	**/
	function getFullYear():Int;

	/**
		Returns the hour (0-23) in the specified date according to local time.
	**/
	function getHours():Int;

	/**
		Returns the milliseconds (0-999) in the specified date according to local time.
	**/
	function getMilliseconds():Int;

	/**
		Returns the minutes (0-59) in the specified date according to local time.
	**/
	function getMinutes():Int;

	/**
		Returns the month (0-11) in the specified date according to local time.
	**/
	function getMonth():Int;

	/**
		Returns the seconds (0-59) in the specified date according to local time.
	**/
	function getSeconds():Int;

	/**
		Returns the numeric value of the specified date as the number of milliseconds since January 1, 1970, 00:00:00 UTC (negative for prior times).
	**/
	function getTime():Float;

	/**
		Returns the time-zone offset in minutes for the current locale.
	**/
	function getTimezoneOffset():Int;

	/**
		Returns the day (date) of the month (1-31) in the specified date according to universal time.
	**/
	function getUTCDate():Int;

	/**
		Returns the day of the week (0-6) in the specified date according to universal time.
	**/
	function getUTCDay():Int;

	/**
		Returns the year (4 digits for 4-digit years) in the specified date according to universal time.
	**/
	function getUTCFullYear():Int;

	/**
		Returns the hours (0-23) in the specified date according to universal time.
	**/
	function getUTCHours():Int;

	/**
		Returns the milliseconds (0-999) in the specified date according to universal time.
	**/
	function getUTCMilliseconds():Int;

	/**
		Returns the minutes (0-59) in the specified date according to universal time.
	**/
	function getUTCMinutes():Int;

	/**
		Returns the month (0-11) in the specified date according to universal time.
	**/
	function getUTCMonth():Int;

	/**
		Returns the seconds (0-59) in the specified date according to universal time.
	**/
	function getUTCSeconds():Int;

	/**
		Sets the day of the month for a specified date according to local time.
	**/
	function setDate(value:Int):Void;

	/**
		Sets the full year (e.g. 4 digits for 4-digit years) for a specified date according to local time.
	**/
	function setFullYear(value:Int):Void;

	/**
		Sets the hours for a specified date according to local time.
	**/
	function setHours(value:Int):Void;

	/**
		Sets the milliseconds for a specified date according to local time.
	**/
	function setMilliseconds(value:Int):Void;

	/**
		Sets the minutes for a specified date according to local time.
	**/
	function setMinutes(value:Int):Void;

	/**
		Sets the month for a specified date according to local time.
	**/
	function setMonth(value:Int):Void;

	/**
		Sets the seconds for a specified date according to local time.
	**/
	function setSeconds(value:Int):Void;

	/**
		Sets the Date object to the time represented by a number of milliseconds since January 1, 1970, 00:00:00 UTC, allowing for negative numbers for times prior.
	**/
	function setTime(value:Float):Void;

	/**
		Sets the day of the month for a specified date according to universal time.
	**/
	function setUTCDate(value:Int):Void;

	/**
		Sets the full year (e.g. 4 digits for 4-digit years) for a specified date according to universal time.
	**/
	function setUTCFullYear(value:Int):Void;

	/**
		Sets the hour for a specified date according to universal time.
	**/
	function setUTCHours(value:Int):Void;

	/**
		Sets the milliseconds for a specified date according to universal time.
	**/
	function setUTCMilliseconds(value:Int):Void;

	/**
		Sets the minutes for a specified date according to universal time.
	**/
	function setUTCMinutes(value:Int):Void;

	/**
		Sets the month for a specified date according to universal time.
	**/
	function setUTCMonth(value:Int):Void;

	/**
		Sets the seconds for a specified date according to universal time.
	**/
	function setUTCSeconds(value:Int):Void;

	/**
		Returns the "date" portion of the Date as a human-readable string.
	**/
	function toDateString():String;

	/**
		Converts a date to a string following the ISO 8601 Extended Format.
	**/
	function toISOString():String;

	/**
		Returns a string representing the Date using toISOString(). Intended for use by JSON.stringify().
	**/
	function toJSON():String;

	/**
		Returns a string with a locality sensitive representation of the date portion of this date based on system settings.
	**/
	@:overload(function(?locales:Array<String>, ?options:Dynamic<Dynamic>):String {})
	function toLocaleDateString(?locales:String, ?options:Dynamic<Dynamic>):String;

	/**
		Converts a date to a string, using a format string.
	**/
	function toLocaleFormat(format:String):String;

	/**
		Returns a string with a locality sensitive representation of this date. Overrides the Object.prototype.toLocaleString() method.
	**/
	@:overload(function(?locales:Array<String>, ?options:Dynamic<Dynamic>):String {})
	function toLocaleString(?locales:String, ?options:Dynamic<Dynamic>):String;

	/**
		Returns a string with a locality sensitive representation of the time portion of this date based on system settings.
	**/
	@:overload(function(?locales:Array<String>, ?options:Dynamic<Dynamic>):String {})
	function toLocaleTimeString(?locales:String, ?options:Dynamic<Dynamic>):String;

	/**
		Returns a string representing the source for an equivalent Date object; you can use this value to create a new object. Overrides the Object.prototype.toSource() method.
	**/
	function toSource():String;

	/**
		Returns a string representing the specified Date object. Overrides the Object.prototype.toString() method.
	**/
	function toString():String;

	/**
		Returns the "time" portion of the Date as a human-readable string.
	**/
	function toTimeString():String;

	/**
		Converts a date to a string using the UTC timezone.
	**/
	function toUTCString():String;
}

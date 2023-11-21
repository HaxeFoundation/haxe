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

/**
	The Date class provides a basic structure for date and time related
	information. Date instances can be created by

	- `new Date()` for a specific date,
	- `Date.now()` to obtain information about the current time,
	- `Date.fromTime()` with a given timestamp or
	- `Date.fromString()` by parsing from a String.

	There are some extra functions available in the `DateTools` class.

	In the context of Haxe dates, a timestamp is defined as the number of
	milliseconds elapsed since 1st January 1970 UTC.

	## Supported range

	Due to platform limitations, only dates in the range 1970 through 2038 are
	supported consistently. Some targets may support dates outside this range,
	depending on the OS at runtime. The `Date.fromTime` method will not work with
	timestamps outside the range on any target.
**/
extern class Date {
	/**
		Creates a new date object from the given arguments.

		The behaviour of a Date instance is only consistent across platforms if
		the the arguments describe a valid date.

		- month: 0 to 11 (note that this is zero-based)
		- day: 1 to 31
		- hour: 0 to 23
		- min: 0 to 59
		- sec: 0 to 59
	**/
	function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void;

	/**
		Returns the timestamp (in milliseconds) of `this` date.
		On cpp and neko, this function only has a second resolution, so the
		result will always be a multiple of `1000.0`, e.g. `1454698271000.0`.
		To obtain the current timestamp with better precision on cpp and neko,
		see the `Sys.time` API.

		For measuring time differences with millisecond accuracy on
		all platforms, see `haxe.Timer.stamp`.
	**/
	function getTime():Float;

	/**
		Returns the hours of `this` Date (0-23 range) in the local timezone.
	**/
	function getHours():Int;

	/**
		Returns the minutes of `this` Date (0-59 range) in the local timezone.
	**/
	function getMinutes():Int;

	/**
		Returns the seconds of `this` Date (0-59 range) in the local timezone.
	**/
	function getSeconds():Int;

	/**
		Returns the full year of `this` Date (4 digits) in the local timezone.
	**/
	function getFullYear():Int;

	/**
		Returns the month of `this` Date (0-11 range) in the local timezone.
		Note that the month number is zero-based.
	**/
	function getMonth():Int;

	/**
		Returns the day of `this` Date (1-31 range) in the local timezone.
	**/
	function getDate():Int;

	/**
		Returns the day of the week of `this` Date (0-6 range, where `0` is Sunday)
		in the local timezone.
	**/
	function getDay():Int;

	/**
		Returns the hours of `this` Date (0-23 range) in UTC.
	**/
	function getUTCHours():Int;

	/**
		Returns the minutes of `this` Date (0-59 range) in UTC.
	**/
	function getUTCMinutes():Int;

	/**
		Returns the seconds of `this` Date (0-59 range) in UTC.
	**/
	function getUTCSeconds():Int;

	/**
		Returns the full year of `this` Date (4 digits) in UTC.
	**/
	function getUTCFullYear():Int;

	/**
		Returns the month of `this` Date (0-11 range) in UTC.
		Note that the month number is zero-based.
	**/
	function getUTCMonth():Int;

	/**
		Returns the day of `this` Date (1-31 range) in UTC.
	**/
	function getUTCDate():Int;

	/**
		Returns the day of the week of `this` Date (0-6 range, where `0` is Sunday)
		in UTC.
	**/
	function getUTCDay():Int;

	/**
		Returns the time zone difference of `this` Date in the current locale
		to UTC, in minutes.

		Assuming the function is executed on a machine in a UTC+2 timezone,
		`Date.now().getTimezoneOffset()` will return `-120`.
	**/
	function getTimezoneOffset():Int;

	/**
		Returns a string representation of `this` Date in the local timezone
		using the standard format `YYYY-MM-DD HH:MM:SS`. See `DateTools.format` for
		other formatting rules.
	**/
	function toString():String;

	/**
		Returns a Date representing the current local time.
	**/
	static function now():Date;

	/**
		Creates a Date from the timestamp (in milliseconds) `t`.
	**/
	static function fromTime(t:Float):Date;

	/**
		Creates a Date from the formatted string `s`. The following formats are
		accepted by the function:

		- `"YYYY-MM-DD hh:mm:ss"`
		- `"YYYY-MM-DD"`
		- `"hh:mm:ss"`

		The first two formats expressed a date in local time. The third is a time
		relative to the UTC epoch.

		If `s` does not match these formats, the result is unspecified.
	**/
	static function fromString(s:String):Date;
}

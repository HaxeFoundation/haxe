/*
 * Copyright (C)2005-2015 Haxe Foundation
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
	milliseconds elapsed since 1st January 1970.
**/
extern class Date
{
	/**
		Creates a new date object from the given arguments.

		The behaviour of a Date instance is only consistent across platforms if
		the the arguments describe a valid date.

		- month: 0 to 11
		- day: 1 to 31
		- hour: 0 to 23
		- min: 0 to 59
		- sec: 0 to 59
	**/
	function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void;

	/**
		Returns the timestamp of the date. It might only have a per-second
		precision depending on the platforms.
	**/
	function getTime() : Float;

	/**
		Returns the hours of `this` Date (0-23 range).
	**/
	function getHours() : Int;

	/**
		Returns the minutes of `this` Date (0-59 range).
	**/
	function getMinutes() : Int;

	/**
		Returns the seconds of the `this` Date (0-59 range).
	**/
	function getSeconds() : Int;

	/**
		Returns the full year of `this` Date (4-digits).
	**/
	function getFullYear() : Int;

	/**
		Returns the month of `this` Date (0-11 range).
	**/
	function getMonth() : Int;

	/**
		Returns the day of `this` Date (1-31 range).
	**/
	function getDate() : Int;

	/**
		Returns the day of the week of `this` Date (0-6 range).
	**/
	function getDay() : Int;

	/**
		Returns a string representation of `this` Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See `DateTools.format` for
		other formating rules.
	**/
	function toString():String;

	/**
		Returns a Date representing the current local time.
	**/
	static function now() : Date;

	/**
		Returns a Date from timestamp `t`.
	**/
	static function fromTime( t : Float ) : Date;

	/**
		Returns a Date from a formated string `s`, with the following accepted
		formats:

		- `"YYYY-MM-DD hh:mm:ss"`
		- `"YYYY-MM-DD"`
		- `"hh:mm:ss"`

		The first two formats are expressed in local time, the third in UTC
		Epoch.
	**/
	static function fromString( s : String ) : Date;
}

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

package python.lib.datetime;

import python.lib.time.StructTime;

@:pythonImport("datetime", "datetime")
extern class Datetime {
	function new(year:Int, month:Int, day:Int, hour:Int = 0, minute:Int = 0, second:Int = 0, microsecond:Int = 0, tzinfo:Tzinfo = null);

	static var min:Datetime;
	static var max:Datetime;
	static var resolution:Timedelta;

	var year:Int;
	var month:Int;
	var day:Int;
	var hour:Int;
	var minute:Int;
	var second:Int;
	var microsecond:Int;
	var tzinfo:Tzinfo;

	static function today():Datetime;
	static function now(?tzinfo:Tzinfo):Datetime;
	static function utcnow():Datetime;
	static function fromtimestamp(timestamp:Float, tzInfo:Tzinfo = null):Datetime;
	static function utcfromtimestamp(timestamp:Int):Datetime;
	static function fromordinal(ordinal:Int):Datetime;

	function timetuple():StructTime;
	function strftime(format:String):String;
	function replace(kwargs:python.KwArgs<{
		?year:Int,
		?month:Int,
		?day:Int,
		?hour:Int,
		?minute:Int,
		?second:Int,
		?microsecond:Int,
		?tzinfo:Tzinfo
	}>):Datetime;
	/* 0-6 */
	function weekday():Int;
	/* 1-7 */
	function isoweekday():Int;
	function utcoffset():Int;

	// python 3.3
	function timestamp():Float;
	function astimezone(?tz:Tzinfo):Datetime;
}

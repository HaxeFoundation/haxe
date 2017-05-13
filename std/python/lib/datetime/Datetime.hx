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
package python.lib.datetime;

import python.lib.time.StructTime;

@:pythonImport("datetime", "datetime")
extern class Datetime {

	public function new (year:Int, month:Int, day:Int, hour:Int=0, minute:Int=0, second:Int=0, microsecond:Int=0, tzinfo:Tzinfo=null);

	public static var min : Datetime;
	public static var max : Datetime;
	public static var resolution : Timedelta;

	public var year : Int;
	public var month : Int;
	public var day : Int;
	public var hour : Int;
	public var minute : Int;
	public var second : Int;
	public var microsecond : Int;
	public var tzinfo : Tzinfo;



	public static function today ():Datetime;
	public static function now (?tzinfo:Tzinfo):Datetime;
	public static function utcnow ():Datetime;
	public static function fromtimestamp (timestamp:Float, tzInfo:Tzinfo=null):Datetime;
	public static function utcfromtimestamp (timestamp:Int):Datetime;
	public static function fromordinal (ordinal:Int):Datetime;

	public function timetuple():StructTime;
	public function strftime (format:String):String;
	public function replace (?year:Int = 1970, ?month:Int = 1, ?day:Int = 1, ?hour:Int = 0, ?minute:Int = 0, ?second:Int, ?microsecond:Int, ?tzinfo:Tzinfo):Datetime;
	/* 0-6 */
	public function weekday():Int;
	/* 1-7 */
	public function isoweekday():Int;

	// python 3.3
	public function timestamp ():Float;
}
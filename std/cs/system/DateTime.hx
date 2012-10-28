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
package cs.system;
import haxe.Int64;

/**
 * ...
 * @author waneck
 */

@:native("System.DateTime")
extern class DateTime 
{

	@:overload(function(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ):Void {})
	function new(ticks:Int64):Void;
	
	var Day(default, null):Int;
	var DayOfWeek(default, null):DayOfWeek;
	var DayOfYear(default, null):Int;
	var Hour(default, null):Int;
	var Millisecond(default, null):Int;
	var Minute(default, null):Int;
	var Second(default, null):Int;
	var Year(default, null):Int;
	var Month(default, null):Int;
	var Ticks(default, null):Int64;
	static var Now(default, null):DateTime;
	static var UtcNow(default, null):DateTime;
}

@:native("System.DayOfWeek")
extern enum DayOfWeek
{
	Sunday;
	Monday;
	Tuesday;
	Wedsneday;
	Thursday;
	Friday;
	Saturday;
}
@:native("System.TimeSpan")
extern class TimeSpan
{
	static var TicksPerMillisecond(default, null):Int;
}
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

import hl.Ref;

#if (hl_ver >= version("2.0.0"))
private typedef DateT = Float; // milliseconds since epoch
#else
private typedef DateT = Int; // seconds since epoch, read as unsigned
#end

@:coreApi final class Date {
	private var t:DateT;

	public function new(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Void {
		t = date_new(year, month, day, hour, min, sec);
	}

	public function getTime():Float {
		#if (hl_ver >= version("2.0.0"))
		return t;
		#else
		return date_get_time(t);
		#end
	}

	public function getFullYear():Int {
		var v = 0;
		date_get_inf(t, v, null, null, null, null, null, null);
		return v;
	}

	public function getMonth():Int {
		var v = 0;
		date_get_inf(t, null, v, null, null, null, null, null);
		return v;
	}

	public function getDate():Int {
		var v = 0;
		date_get_inf(t, null, null, v, null, null, null, null);
		return v;
	}

	public function getHours():Int {
		var v = 0;
		date_get_inf(t, null, null, null, v, null, null, null);
		return v;
	}

	public function getMinutes():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, v, null, null);
		return v;
	}

	public function getSeconds():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, v, null);
		return v;
	}

	public function getDay():Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, null, v);
		return v;
	}

	public function getUTCFullYear():Int {
		var v = 0;
		date_get_utc_inf(t, v, null, null, null, null, null, null);
		return v;
	}

	public function getUTCMonth():Int {
		var v = 0;
		date_get_utc_inf(t, null, v, null, null, null, null, null);
		return v;
	}

	public function getUTCDate():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, v, null, null, null, null);
		return v;
	}

	public function getUTCHours():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, v, null, null, null);
		return v;
	}

	public function getUTCMinutes():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, v, null, null);
		return v;
	}

	public function getUTCSeconds():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, null, v, null);
		return v;
	}

	public function getUTCDay():Int {
		var v = 0;
		date_get_utc_inf(t, null, null, null, null, null, null, v);
		return v;
	}

	public function getTimezoneOffset():Int {
		var y = 0;
		var mo = 0;
		var d = 0;
		var h = 0;
		var m = 0;
		var s = 0;
		date_get_utc_inf(t, y, mo, d, h, m, s, null);
		#if (hl_ver >= version("2.0.0"))
		return Std.int((date_new(y, mo, d, h, m, s) - Math.ffloor(t / 1000) * 1000) / 60000);
		#else
		return Std.int((date_new(y, mo, d, h, m, s) - t) / 60);
		#end
	}

	@:keep public function toString():String {
		var outLen = 0;
		var bytes = date_to_string(t, outLen);
		return @:privateAccess String.__alloc__(bytes, outLen);
	}

	public static function now():Date {
		var d:Date = untyped $new(Date);
		d.t = date_now();
		return d;
	}

	static function fromInt(t:Int):Date {
		#if (hl_ver >= version("2.0.0"))
		return fromTime((t : haxe.UInt32) * 1000.);
		#else
		var d:Date = untyped $new(Date);
		d.t = t;
		return d;
		#end
	}

	public static function fromTime(t:Float):Date {
		var d:Date = untyped $new(Date);
		#if (hl_ver >= version("2.0.0"))
		d.t = t;
		#else
		d.t = date_from_time(t);
		#end
		return d;
	}

	public static function fromString(s:String):Date {
		var d:Date = untyped $new(Date);
		d.t = date_from_string(@:privateAccess s.bytes, s.length << 1);
		return d;
	}

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_new2") #else @:hlNative #end
	static function date_new(year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int):DateT {
		return 0;
	}

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_now2") #else @:hlNative #end
	static function date_now():DateT {
		return 0;
	}

	#if (hl_ver < version("2.0.0"))
	@:hlNative
	static function date_from_time(t:Float):Int {
		return 0;
	}

	@:hlNative
	static function date_get_time(t:Int):Float {
		return 0.;
	}
	#end

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_from_string2") #else @:hlNative #end
	static function date_from_string(b:hl.Bytes, len:Int):DateT {
		return 0;
	}

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_get_inf2") #else @:hlNative #end
	static function date_get_inf(t:DateT, year:Ref<Int>, month:Ref<Int>, day:Ref<Int>, hours:Ref<Int>, minutes:Ref<Int>, seconds:Ref<Int>, wday:Ref<Int>):Void {}

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_get_utc_inf2") #else @:hlNative(1.11) #end
	static function date_get_utc_inf(t:DateT, year:Ref<Int>, month:Ref<Int>, day:Ref<Int>, hours:Ref<Int>, minutes:Ref<Int>, seconds:Ref<Int>, wday:Ref<Int>):Void {}

	#if (hl_ver >= version("2.0.0")) @:hlNative("std", "date_to_string2") #else @:hlNative #end
	static function date_to_string(t:DateT, outLen:Ref<Int>):hl.Bytes {
		return null;
	}
}

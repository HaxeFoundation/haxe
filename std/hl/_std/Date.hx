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
import hl.Ref;

@:coreApi @:final class Date {

	private var t : Int;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void {
		t = date_new(year, month, day, hour, min, sec);
	}

	public function getTime() : Float {
		return date_get_time(t);
	}

	public function getFullYear() : Int {
		var v = 0;
		date_get_inf(t, v, null, null, null, null, null, null);
		return v;
	}

	public function getMonth() : Int {
		var v = 0;
		date_get_inf(t, null, v, null, null, null, null, null);
		return v;
	}

	public function getDate() : Int {
		var v = 0;
		date_get_inf(t, null, null, v, null, null, null, null);
		return v;
	}

	public function getHours() : Int {
		var v = 0;
		date_get_inf(t, null, null, null, v, null, null, null);
		return v;
	}

	public function getMinutes() : Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, v, null, null);
		return v;
	}

	public function getSeconds() : Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, v, null);
		return v;
	}

	public function getDay() : Int {
		var v = 0;
		date_get_inf(t, null, null, null, null, null, null, v);
		return v;
	}

	@:keep public function toString():String {
		var outLen = 0;
		var bytes = date_to_string(t, outLen);
		return @:privateAccess String.__alloc__(bytes,outLen);
	}

	public static function now() : Date {
		var d : Date = untyped $new(Date);
		d.t = date_now();
		return d;
	}

	static function fromInt( t : Int ) : Date {
		var d : Date = untyped $new(Date);
		d.t = t;
		return d;
	}

	public static function fromTime( t : Float ) : Date {
		var d : Date = untyped $new(Date);
		d.t = date_from_time(t);
		return d;
	}

	public static function fromString( s : String ) : Date {
		var d : Date = untyped $new(Date);
		d.t = date_from_string(@:privateAccess s.bytes, s.length<<1);
		return d;
	}

	@:hlNative
	static function date_new( year : Int, month : Int, day : Int, hours : Int, minutes : Int, seconds : Int ) : Int {
		return 0;
	}

	@:hlNative
	static function date_now() : Int {
		return 0;
	}

	@:hlNative
	static function date_from_time( t : Float ) : Int {
		return 0;
	}

	@:hlNative
	static function date_from_string( b : hl.Bytes, len : Int ) : Int {
		return 0;
	}

	@:hlNative
	static function date_get_time( t : Int ) : Float {
		return 0.;
	}

	@:hlNative
	static function date_get_inf( t : Int, year : Ref<Int>, month : Ref<Int>, day : Ref<Int>, hours : Ref<Int>, minutes : Ref<Int>, seconds : Ref<Int>, wday : Ref<Int> ) : Void {
	}

	@:hlNative
	static function date_to_string( t : Int, outLen : Ref<Int> ) : hl.Bytes {
		return null;
	}

}



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
import neko.Lib;

@:coreApi @:final class Date {

	private var __t : Dynamic;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void {
		__t = date_set_day(0,year,month+1,day);
		__t = date_set_hour(__t,hour,min,sec);
	}

	public function getTime() : Float {
		return int32_to_float(__t) * 1000;
	}

	public function getFullYear() : Int {
		return date_get_day(__t).y;
	}

	public function getMonth() : Int {
		return date_get_day(__t).m -1;
	}

	public function getDate() : Int {
		return date_get_day(__t).d;
	}

	public function getHours() : Int {
		return date_get_hour(__t).h;
	}

	public function getMinutes() : Int {
		return date_get_hour(__t).m;
	}

	public function getSeconds() : Int {
		return date_get_hour(__t).s;
	}

	public function getDay() : Int {
		return Std.parseInt( new String(date_format(__t,untyped "%w".__s)) );
	}

	@:keep public function toString():String {
		return new String(date_format(__t,null));
	}

	public static function now() : Date {
		return new1(date_now());
	}

	public static function fromTime( t : Float ) : Date {
		t /= 1000;
		var i1 = untyped __dollar__int((t%65536));
		var i2 = untyped __dollar__int(t/65536);
		var i = int32_add(i1,int32_shl(i2,16));
		return new1(i);
	}

	public static function fromString( s : String ) : Date {
		return new1(date_new(untyped s.__s));
	}

	private static function new1(t : Dynamic) : Date {
		var d = new Date(2005,1,1,0,0,0);
		d.__t = t;
		return d;
	}

	static var date_new = Lib.load("std","date_new",1);
	static var date_now = Lib.load("std","date_now",0);
	static var date_format = Lib.load("std","date_format",2);
	static var date_set_hour = Lib.load("std","date_set_hour",4);
	static var date_set_day = Lib.load("std","date_set_day",4);
	static var date_get_day : Dynamic -> {y:Int, m:Int, d:Int} = Lib.load("std","date_get_day",1);
	static var date_get_hour : Dynamic -> {h:Int, m:Int, s:Int} = Lib.load("std","date_get_hour",1);
	static var int32_to_float = Lib.load("std","int32_to_float",1);
	static var int32_add = Lib.load("std","int32_add",2);
	static var int32_shl = Lib.load("std","int32_shl",2);
	@:keep static function __string() : String { return untyped "Date".__s; }

}



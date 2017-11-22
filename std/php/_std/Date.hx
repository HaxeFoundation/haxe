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

import php.Global.*;
import php.Syntax.*;

@:coreApi @:final class Date
{
	private var __t : Float;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void {
		__t = mktime(hour, min, sec, month + 1, day, year);
	}

	public function getTime() : Float {
		return __t * 1000.0;
	}

	private function getPhpTime() : Float {
		return __t;
	}

	public function getFullYear() : Int {
		return int(date("Y", int(__t)));
	}

	public function getMonth() : Int {
		var m : Int = int(date("n", int(__t)));
		return -1 + m;
	}

	public function getDate() : Int {
		return int(date("j", int(__t)));
	}

	public function getHours() : Int {
		return int(date("G", int(__t)));
	}

	public function getMinutes() : Int {
		return int(date("i", int(__t)));
	}

	public function getSeconds() : Int {
		return int(date("s", int(__t)));
	}

	public function getDay() : Int {
		return int(date("w", int(__t)));
	}

	public function toString():String {
		return date("Y-m-d H:i:s", int(__t));
	}

	public static function now() : Date {
		return fromPhpTime(round(microtime(true), 3));
	}

	static function fromPhpTime( t : Float ) : Date {
		var d = new Date(2000, 1, 1, 0, 0, 0);
		d.__t = t;
		return d;
	}

	public static function fromTime( t : Float ) : Date {
		var d = new Date(2000, 1, 1, 0, 0, 0);
		d.__t = t / 1000;
		return d;
	}

	public static function fromString( s : String ) : Date {
		return fromPhpTime(strtotime(s));
	}
}



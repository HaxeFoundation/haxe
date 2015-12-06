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

@:coreApi @:final class Date {

	private var t : Int;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void {
		throw "TODO";
	}

	public function getTime() : Float {
		throw "TODO";
		return 0.;
	}

	public function getFullYear() : Int {
		throw "TODO";
		return 0;
	}

	public function getMonth() : Int {
		throw "TODO";
		return 0;
	}

	public function getDate() : Int {
		throw "TODO";
		return 0;
	}

	public function getHours() : Int {
		throw "TODO";	
		return 0;
	}

	public function getMinutes() : Int {
		throw "TODO";
		return 0;
	}

	public function getSeconds() : Int {
		throw "TODO";
		return 0;
	}

	public function getDay() : Int {
		throw "TODO";
		return 0;
	}

	@:keep public function toString():String {
		throw "TODO";
		return "";
	}

	public static function now() : Date {
		throw "TODO";
		return null;
	}

	public static function fromTime( t : Float ) : Date {
		throw "TODO";
		return null;
	}

	public static function fromString( s : String ) : Date {
		throw "TODO";
		return null;
	}

}



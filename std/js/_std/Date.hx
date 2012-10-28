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
@:coreApi extern class Date {

	function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void;
	function getTime() : Float;
	function getHours() : Int;
	function getMinutes() : Int;
	function getSeconds() : Int;
	function getFullYear() : Int;
	function getMonth() : Int;
	function getDate() : Int;
	function getDay() : Int;

	inline function toString() : String {
		return untyped HxOverrides.dateStr(this);
	}

	static inline function now() : Date {
		return untyped __new__(Date);
	}

	static inline function fromTime( t : Float ) : Date {
		var d : Date = untyped __new__(Date);
		untyped d["setTime"]( t );
		return d;
	}

	static inline function fromString( s : String ) : Date {
		return untyped HxOverrides.strDate(s);
	}
}

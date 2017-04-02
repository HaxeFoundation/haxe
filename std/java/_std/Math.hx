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

@:coreApi
@:native("java.lang.Math") extern class Math
{
	static var PI(default,null) : Float;
	static var NaN(default,null) : Float;
	static var NEGATIVE_INFINITY(default,null) : Float;
	static var POSITIVE_INFINITY(default,null) : Float;

	static function abs(v:Float):Float;
	static function min(a:Float,b:Float):Float;
	static function max(a:Float,b:Float):Float;
	static function sin(v:Float):Float;
	static function cos(v:Float):Float;
	static function atan2(y:Float,x:Float):Float;
	static function tan(v:Float):Float;
	static function exp(v:Float):Float;
	static function log(v:Float):Float;
	static function sqrt(v:Float):Float;
	static function round(v:Float):Int;
	static function floor(v:Float):Int;
	static function ceil(v:Float):Int;
	static function atan(v:Float):Float;
	inline static function fround(v:Float):Float {
		return ffloor(v + 0.5);
	}
	static function ffloor(v:Float):Float;
	static function fceil(v:Float):Float;
	static function asin(v:Float):Float;
	static function acos(v:Float):Float;
	static function pow(v:Float,exp:Float):Float;
	static function random() : Float;

	static function isFinite( f : Float ) : Bool;
	static function isNaN( f : Float ) : Bool;
}



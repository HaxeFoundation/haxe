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
@:coreApi class Math
{
	public static var PI(default,null) : Float;
	public static var NaN(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;

	public static function abs(v : Float) : Float      { return untyped __call__("abs", v); }
	public static function min(a : Float,b : Float) : Float    { return untyped !isNaN(a) ? __call__("min", a, b) : NaN; }
	public static function max(a : Float,b : Float) : Float    { return untyped !isNaN(b) ? __call__("max", a, b) : NaN; }
	public static function sin(v : Float) : Float      { return untyped __call__("sin", v); }
	public static function cos(v : Float) : Float      { return untyped __call__("cos", v); }
	public static function atan2(y : Float,x : Float) : Float  { return untyped __call__("atan2", y, x); }
	public static function tan(v : Float) : Float      { return untyped __call__("tan", v); }
	public static function exp(v : Float) : Float      {
		if(isNaN(v))
			return NaN;
		if(untyped __call__("is_finite", v))
			return untyped __call__("exp", v);
		else if(v == POSITIVE_INFINITY)
			return POSITIVE_INFINITY;
		else
			return 0.0;
	}
	public static function log(v : Float) : Float      { return untyped __call__("log", v); }
	public static function sqrt(v : Float) : Float     { return untyped __call__("sqrt", v); }
	public static function round(v : Float) : Int      { return untyped __call__("(int) floor", v + 0.5); }
	public static function floor(v : Float) : Int      { return untyped __call__("(int) floor", v); }
	public static function ceil(v : Float) : Int       { return untyped __call__("(int) ceil", v); }
	public static function atan(v : Float) : Float     { return untyped __call__("atan", v); }
	public static function asin(v : Float) : Float     { return untyped __call__("asin", v); }
	public static function acos(v : Float) : Float     { return untyped __call__("acos", v); }
	public static function pow(v : Float,exp : Float) : Float    { return untyped __call__("pow", v, exp); }
	public static function random() : Float    { return untyped __call__("mt_rand") / __call__("mt_getrandmax"); }
	public static function isNaN(f : Float) : Bool     { return untyped __call__("is_nan", f); }
	public static function isFinite(f : Float) : Bool  { return untyped __call__("is_finite", f); }

	public static function fround(v : Float) : Float      { return untyped __call__("floor", v + 0.5); }
	public static function ffloor(v : Float) : Float      { return untyped __call__("floor", v); }
	public static function fceil(v : Float) : Float       { return untyped __call__("ceil", v); }

	static function __init__() : Void {
	 	PI = untyped __php__("M_PI");
	 	NaN = untyped __php__("acos(1.01)");
	 	NEGATIVE_INFINITY = untyped __php__("log(0)");
	 	POSITIVE_INFINITY = -NEGATIVE_INFINITY;
	}

}



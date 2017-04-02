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
import python.internal.UBuiltins;

@:pythonImport("math")
@:coreApi
extern class Math
{
	static var PI(default,null) : Float;

	static var NEGATIVE_INFINITY(default, null) : Float;

	static var POSITIVE_INFINITY(default,null) : Float;

	static var NaN(default, null) : Float;

	public static inline function abs(v:Float):Float
	{
		return (Math:Dynamic).fabs(v);
	}

	public static inline function min(a:Float, b:Float):Float {
		return if (isNaN(a)) a else if (isNaN(b)) b else UBuiltins.min(a,b);
	}

	public static inline function max(a:Float, b:Float):Float
	{
		return if (isNaN(a)) a else if (isNaN(b)) b else UBuiltins.max(a,b);
	}

	public static inline function sin(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.sin(v);
	}

	public static inline function cos(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.cos(v);
	}

	static function tan(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.tan(v);
	}
	static function asin(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.asin(v);
	}
	static function acos(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.acos(v);
	}
	static function atan(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.atan(v);
	}
	static function atan2(y:Float, x:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.atan2(v);
	}

	public static inline function exp(v:Float):Float
	{
		if (v == NEGATIVE_INFINITY) {
			return 0.0;
		} else if (v == POSITIVE_INFINITY) {
			return POSITIVE_INFINITY;
		} else {
			return (Math:Dynamic).exp(v);
		}
	}

	public static inline function log(v:Float):Float {
		return if (v == 0.0) NEGATIVE_INFINITY else if (v < 0.0) NaN else python.lib.Math.log(v);
	}

	static function pow(v:Float, exp:Float):Float;

	public static inline function sqrt(v:Float):Float
	{
		return if (v < 0) NaN else python.lib.Math.sqrt(v);
	}

	public static inline function round(v:Float):Int {
		return Math.floor(v + 0.5);
	}

	static function floor(v:Float):Int;

	static function ceil(v:Float):Int;

	inline static function random() : Float {
		return python.lib.Random.random();
	}

	static inline function ffloor( v : Float ) : Float {
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return floor(v);
	}

	static inline function fceil( v : Float ) : Float
	{
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return ceil(v);
	}

	static inline function fround( v : Float ) : Float {
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return round(v);
	}

	static inline function isFinite( f : Float ) : Bool return f != POSITIVE_INFINITY && f != NEGATIVE_INFINITY && !isNaN(f);

	static inline function isNaN( f : Float ) : Bool {

		return python.lib.Math.isnan(f);
	}

	static function __init__():Void {
		NEGATIVE_INFINITY = UBuiltins.float('-inf');
		POSITIVE_INFINITY = UBuiltins.float('inf');
		NaN = UBuiltins.float("nan");
		PI = python.lib.Math.pi;
	}

}
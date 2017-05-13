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

@:native("Math")
@:keep
private class MathImpl {
	static var __rnd;
	static var _rand_float = Lib.load("std","random_float",1);
	static var _rand_int = Lib.load("std","random_int",2);

	public static function min(a:Float,b:Float) : Float { return if( a < b ) a else if( untyped $isnan(a) ) a else b; }
	public static function max(a:Float,b:Float) : Float { return if( a < b ) b else if( untyped $isnan(b) ) b else a; }
	public static function random() : Float { return _rand_float(__rnd); }
	public static function isNaN(f:Float) : Bool { return untyped __dollar__isnan(f); }
	public static function isFinite(f:Float) : Bool { return !untyped (__dollar__isinfinite(f) || __dollar__isnan(f)); }

	static function __init__() : Void {
		var M : Dynamic = MathImpl;
	 	__rnd = Lib.load("std","random_new",0)();
	 	M.PI = Lib.load("std","math_pi",0)();
	 	M.NaN = 0.0 / 0.0;
	 	M.POSITIVE_INFINITY = 1.0 / 0.0;
	 	M.NEGATIVE_INFINITY = -M.POSITIVE_INFINITY;
		M.abs = Lib.load("std","math_abs",1);
		M.sin = Lib.load("std","math_sin",1);
		M.cos = Lib.load("std","math_cos",1);
		M.atan2 = Lib.load("std","math_atan2",2);
		M.tan = Lib.load("std","math_tan",1);
		M.exp = Lib.load("std","math_exp",1);
		M.log = Lib.load("std","math_log",1);
		M.sqrt = Lib.load("std","math_sqrt",1);
		M.round = Lib.load("std","math_round",1);
		M.floor = Lib.load("std","math_floor",1);
		M.ceil = Lib.load("std","math_ceil",1);
		M.atan = Lib.load("std","math_atan",1);
		M.asin = Lib.load("std","math_asin",1);
		M.acos = Lib.load("std","math_acos",1);
		M.pow = Lib.load("std", "math_pow", 2);
		M.fceil = try Lib.load("std", "math_fceil", 1) catch( e : Dynamic ) M.ceil;
		M.ffloor = try Lib.load("std", "math_ffloor", 1) catch( e : Dynamic ) M.floor;
		M.fround = try Lib.load("std", "math_fround", 1) catch( e : Dynamic ) M.round;
	}
}

@:coreApi
@:final
extern class Math {

	public static var PI(default,null) : Float;
	public static var NaN(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;

	public static function min(a:Float,b:Float) : Float;
	public static function max(a:Float,b:Float) : Float;
	public static function abs( v : Float ) : Float;
	public static function sin( v : Float ) : Float;
	public static function cos( v : Float ) : Float;
	public static function atan2( y : Float, x : Float ) : Float;
	public static function tan( v : Float ) : Float;
	public static function exp( v : Float ) : Float;
	public static function log( v : Float ) : Float;
	public static function sqrt( v : Float ) : Float;
	public static function round( v : Float ) : Int;
	public static function floor( v : Float ) : Int;
	public static function ceil( v : Float ) : Int;
	public static function atan( v : Float ) : Float;
	public static function asin( v : Float ) : Float;
	public static function acos( v : Float ) : Float;
	public static function pow( v : Float, exp : Float ) : Float;
	public static function fround( v : Float ) : Float;
	public static function ffloor( v : Float ) : Float;
	public static function fceil( v : Float ) : Float;
	public static function random() : Float;
	public static function isNaN(f:Float) : Bool;
	public static function isFinite(f:Float) : Bool;
}
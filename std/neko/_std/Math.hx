/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
import neko.Lib;

@:coreApi @:final class Math {

	public static var PI(default,null) : Float;
	public static var NaN(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;

	public static function min(a:Float,b:Float) : Float { return if( a < b ) a else b; }
	public static function max(a:Float,b:Float) : Float { return if( a < b ) b else a; }

	public static function abs( v : Float ) : Float return 0.
	public static function sin( v : Float ) : Float return 0.
	public static function cos( v : Float ) : Float return 0.
	public static function atan2( y : Float, x : Float ) : Float return 0.
	public static function tan( v : Float ) : Float return 0.
	public static function exp( v : Float ) : Float return 0.
	public static function log( v : Float ) : Float return 0.
	public static function sqrt( v : Float ) : Float return 0.
	public static function round( v : Float ) : Int return 0
	public static function floor( v : Float ) : Int return 0
	public static function ceil( v : Float ) : Int return 0
	public static function atan( v : Float ) : Float return 0.
	public static function asin( v : Float ) : Float return 0.
	public static function acos( v : Float ) : Float return 0.
	public static function pow( v : Float, exp : Float ) : Float return 0.

	static var __rnd;
	static var _rand_float = Lib.load("std","random_float",1);
	static var _rand_int = Lib.load("std","random_int",2);

	public static function random() : Float { return _rand_float(__rnd); }

	public static function isNaN(f:Float) : Bool { return untyped __dollar__isnan(f); }
	public static function isFinite(f:Float) : Bool { return !untyped (__dollar__isinfinite(f) || __dollar__isnan(f)); }

	static function __init__() : Void {
	 	__rnd = Lib.load("std","random_new",0)();
	 	PI = Lib.load("std","math_pi",0)();
	 	NaN = 0.0 / 0.0;
	 	POSITIVE_INFINITY = 1.0 / 0.0;
	 	NEGATIVE_INFINITY = -POSITIVE_INFINITY;
		var M : Dynamic = Math;
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
		M.pow = Lib.load("std","math_pow",2);
	}

}



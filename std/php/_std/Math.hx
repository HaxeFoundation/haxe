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

import php.Global;
import php.Const;
import php.Syntax.*;

@:coreApi class Math {
	public static var PI(default,null) : Float = Const.M_PI;
	public static var NaN(default,null) : Float = Const.NAN;
	public static var POSITIVE_INFINITY(default,null) : Float = Const.INF;
	public static var NEGATIVE_INFINITY(default,null) : Float = -Const.INF;

	public static inline function abs( v:Float ) : Float return Global.abs(v);
	public static inline function min( a:Float, b:Float ) : Float return isNaN(a) || isNaN(b) ? NaN : Global.min(a, b);
	public static inline function max( a:Float, b:Float ) : Float return isNaN(a) || isNaN(b) ? NaN : Global.max(a, b);
	public static inline function sin( v:Float ) : Float return Global.sin(v);
	public static inline function cos( v:Float ) : Float return Global.cos(v);
	public static inline function atan2( y:Float, x:Float ) : Float return Global.atan2(y, x);
	public static inline function tan( v:Float ) : Float return Global.tan(v);
	public static inline function exp( v:Float ) : Float return Global.exp(v);
	public static inline function log( v:Float ) : Float return Global.log(v);
	public static inline function sqrt( v:Float ) : Float return Global.sqrt(v);
	public static inline function round( v:Float ) : Int return int(Global.floor(v + 0.5));
	public static inline function floor( v:Float ) : Int return int(Global.floor(v));
	public static inline function ceil( v:Float ) : Int return int(Global.ceil(v));
	public static inline function atan( v:Float ) : Float return Global.atan(v);
	public static inline function asin( v:Float ) : Float return Global.asin(v);
	public static inline function acos( v:Float ) : Float return Global.acos(v);
	public static inline function pow( v:Float, exp:Float ) : Float return Global.pow(v, exp);
	public static inline function random() : Float return Global.mt_rand() / Global.mt_getrandmax();
	public static inline function isNaN( f:Float ) : Bool return Global.is_nan(f);
	public static inline function isFinite( f:Float ) : Bool return Global.is_finite(f);

	public static inline function fround( v:Float ) : Float return Global.floor(v + 0.5);
	public static inline function ffloor( v:Float ) : Float return Global.floor(v);
	public static inline function fceil( v:Float ) : Float return Global.ceil(v);
}



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
package;

class Math
{
	public static var PI(get,null) : Float;
	static inline function get_PI () : Float return lua.Math.pi;

	public static var NEGATIVE_INFINITY(get, null) : Float;
	static inline function get_NEGATIVE_INFINITY () : Float return -lua.Math.huge;

	public static var POSITIVE_INFINITY(get,null) : Float;
	static inline function get_POSITIVE_INFINITY () : Float return lua.Math.huge;

	public static var NaN(get, null) : Float;

	// Note: this has to be an untyped literal, otherwise the compiler tries
	// to unify it to an Int, which defeats useful numeric reflection behavior.
	static inline function get_NaN () : Float return untyped __lua__("(0/0)");

	public static function isNaN( f : Float ) : Bool return (f != f);
	public static inline function isFinite( f : Float ) : Bool {
		return (f > Math.NEGATIVE_INFINITY && f < Math.POSITIVE_INFINITY);
	}

	public static inline function abs(v:Float) :Float return lua.Math.abs(v);
	public static inline function acos(v:Float):Float return lua.Math.acos(v);
	public static inline function asin(v:Float):Float return lua.Math.asin(v);
	public static inline function atan(v:Float):Float return lua.Math.atan(v);
	public static inline function ceil(v:Float):Int   return lua.Math.ceil(v);
	public static inline function cos(v:Float):Float  return lua.Math.cos(v);
	public static inline function exp(v:Float):Float  return lua.Math.exp(v);
	public static inline function sin(v:Float):Float  return lua.Math.sin(v);
	public static inline function sqrt(v:Float):Float return lua.Math.sqrt(v);
	public static inline function tan(v:Float):Float  return lua.Math.tan(v);

	public static inline function floor(v:Float):Int  return lua.Math.floor(v);
	public static inline function log(v:Float):Float  return lua.Math.log(v);

	public static inline function random() : Float return untyped __define_feature__("Math.random", lua.Math.random());

	public static inline function atan2(y:Float, x:Float):Float return lua.Math.atan2(y,x);
	public static inline function max(a:Float, b:Float):Float {
		return Math.isNaN(a) || Math.isNaN(b) ? Math.NaN : lua.Math.max(a,b);
	}
	public static inline function min(a:Float, b:Float):Float {
		return Math.isNaN(a) || Math.isNaN(b) ? Math.NaN : lua.Math.min(a,b);
	}
	public static inline function pow(v:Float, exp:Float):Float return lua.Math.pow(v,exp);

	public static inline function round(v:Float):Int return Math.floor(v + 0.5);

	public static inline function ffloor( v : Float ) : Float return floor(v);
	public static inline function fceil( v : Float ) : Float return ceil(v);
	public static inline function fround( v : Float ) : Float return round(v);

}

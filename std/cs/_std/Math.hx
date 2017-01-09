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
@:coreApi @:nativeGen class Math
{
	@:readOnly
	private static var rand = new cs.system.Random();

	@:readOnly
	public static var PI(default,null) = cs.system.Math.PI;
	@:readOnly
	public static var NaN(default,null) = cs.system.Double.NaN;
	@:readOnly
	public static var NEGATIVE_INFINITY(default,null) = cs.system.Double.NegativeInfinity;
	@:readOnly
	public static var POSITIVE_INFINITY(default,null) = cs.system.Double.PositiveInfinity;

	public static inline function abs(v:Float):Float
	{
		return cs.system.Math.Abs(v);
	}

	public static inline function min(a:Float, b:Float):Float
	{
		return cs.system.Math.Min(a,b);
	}

	public static inline function max(a:Float, b:Float):Float
	{
		return cs.system.Math.Max(a,b);
	}

	public static inline function sin(v:Float):Float
	{
		return cs.system.Math.Sin(v);
	}

	public static inline function cos(v:Float):Float
	{
		return cs.system.Math.Cos(v);
	}

	public static inline function atan2(y:Float, x:Float):Float
	{
		return cs.system.Math.Atan2(y, x);
	}

	public static inline function tan(v:Float):Float
	{
		return cs.system.Math.Tan(v);
	}

	public static inline function exp(v:Float):Float
	{
		return cs.system.Math.Exp(v);
	}

	public static inline function log(v:Float):Float
	{
		return cs.system.Math.Log(v);
	}

	public static inline function sqrt(v:Float):Float
	{
		return cs.system.Math.Sqrt(v);
	}

	public static inline function fround(v:Float):Float
	{
		return cs.system.Math.Floor(v + 0.5);
	}

	public static inline function ffloor(v:Float):Float
	{
		return cs.system.Math.Floor(v);
	}

	public static inline function fceil(v:Float):Float
	{
		return cs.system.Math.Ceiling(v);
	}

	public static function round(v:Float):Int
	{
		var vint = Std.int(v);
		var dec = v - vint;
		if (dec >= 1 || dec <= -1)
			return vint; //overflow
		if (dec >= .5)
			return vint + 1;
		if (dec < -.5)
			return vint - 1;
		return vint;
	}

	public static inline function floor(v:Float):Int
	{
		return Std.int(cs.system.Math.Floor(v));
	}

	public static inline function ceil(v:Float):Int
	{
		return Std.int(cs.system.Math.Ceiling(v));
	}

	public static inline function atan(v:Float):Float
	{
		return cs.system.Math.Atan(v);
	}

	public static inline function asin(v:Float):Float
	{
		return cs.system.Math.Asin(v);
	}

	public static inline function acos(v:Float):Float
	{
		return cs.system.Math.Acos(v);
	}

	public static inline function pow(v:Float, exp:Float):Float
	{
		return cs.system.Math.Pow(v, exp);
	}

	public static inline function random() : Float
	{
		return rand.NextDouble();
	}

	public static inline function isFinite( f : Float ) : Bool
	{
		return !cs.system.Double.IsInfinity(f) && !cs.system.Double.IsNaN(f);
	}

	public static inline function isNaN( f : Float ) : Bool
	{
		return cs.system.Double.IsNaN(f);
	}
}

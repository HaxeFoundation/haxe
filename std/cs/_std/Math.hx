/*
 * Copyright (C)2005-2019 Haxe Foundation
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
class Math {
	public static var PI(default, null):Float = 3.14159265358979323846;
	public static var NaN(default, null):Float = 0.0 / 0.0;
	public static var NEGATIVE_INFINITY(default, null):Float = -1.0 / 0.0;
	public static var POSITIVE_INFINITY(default, null):Float = 1.0 / 0.0;

	public static inline function abs(v:Float):Float {
		return untyped __cs__("System.Math.Abs({0})", v);
	}

	public static inline function min(a:Float, b:Float):Float {
		return untyped __cs__("System.Math.Min({0}, {1})", a, b);
	}

	public static inline function max(a:Float, b:Float):Float {
		return untyped __cs__("System.Math.Max({0}, {1})", a, b);
	}

	public static inline function sin(v:Float):Float {
		return untyped __cs__("System.Math.Sin({0})", v);
	}

	public static inline function cos(v:Float):Float {
		return untyped __cs__("System.Math.Cos({0})", v);
	}

	public static inline function tan(v:Float):Float {
		return untyped __cs__("System.Math.Tan({0})", v);
	}

	public static inline function asin(v:Float):Float {
		return untyped __cs__("System.Math.Asin({0})", v);
	}

	public static inline function acos(v:Float):Float {
		return untyped __cs__("System.Math.Acos({0})", v);
	}

	public static inline function atan(v:Float):Float {
		return untyped __cs__("System.Math.Atan({0})", v);
	}

	public static inline function atan2(y:Float, x:Float):Float {
		return untyped __cs__("System.Math.Atan2({0}, {1})", y, x);
	}

	public static inline function exp(v:Float):Float {
		return untyped __cs__("System.Math.Exp({0})", v);
	}

	public static inline function log(v:Float):Float {
		return untyped __cs__("System.Math.Log({0})", v);
	}

	public static inline function pow(v:Float, exp:Float):Float {
		return untyped __cs__("System.Math.Pow({0}, {1})", v, exp);
	}

	public static inline function sqrt(v:Float):Float {
		return untyped __cs__("System.Math.Sqrt({0})", v);
	}

	public static inline function round(v:Float):Int {
		return untyped __cs__("(int)System.Math.Round({0})", v);
	}

	public static inline function floor(v:Float):Int {
		return untyped __cs__("(int)System.Math.Floor({0})", v);
	}

	public static inline function ceil(v:Float):Int {
		return untyped __cs__("(int)System.Math.Ceiling({0})", v);
	}

	public static inline function fround(v:Float):Float {
		return untyped __cs__("System.Math.Round({0})", v);
	}

	public static inline function ffloor(v:Float):Float {
		return untyped __cs__("System.Math.Floor({0})", v);
	}

	public static inline function fceil(v:Float):Float {
		return untyped __cs__("System.Math.Ceiling({0})", v);
	}

	public static inline function random():Float {
		return cs.Boot.random();
	}

	public static inline function isFinite(f:Float):Bool {
		return untyped __cs__("!double.IsInfinity({0}) && !double.IsNaN({0})", f);
	}

	public static inline function isNaN(f:Float):Bool {
		return untyped __cs__("double.IsNaN({0})", f);
	}
}

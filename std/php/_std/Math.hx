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

class Math
{
	public static var PI;
	public static var NaN;
	public static var POSITIVE_INFINITY;
	public static var NEGATIVE_INFINITY;

	public static function abs(v) : Float      { return untyped __call__("abs", v); }
	public static function min(a,b) : Float    { return untyped __call__("min", a, b); }
	public static function max(a,b) : Float    { return untyped __call__("max", a, b); }
	public static function sin(v) : Float      { return untyped __call__("sin", v); }
	public static function cos(v) : Float      { return untyped __call__("cos", v); }
	public static function atan2(y,x) : Float  { return untyped __call__("atan2", y, x); }
	public static function tan(v) : Float      { return untyped __call__("tan", v); }
	public static function exp(v) : Float      { return untyped __call__("exp", v); }
	public static function log(v) : Float      { return untyped __call__("log", v); }
	public static function sqrt(v) : Float     { return untyped __call__("sqrt", v); }
	public static function round(v) : Int      { return untyped __call__("(int) floor", v + 0.5); }
	public static function floor(v) : Int      { return untyped __call__("(int) floor", v); }
	public static function ceil(v) : Int       { return untyped __call__("(int) ceil", v); }
	public static function atan(v) : Float     { return untyped __call__("atan", v); }
	public static function asin(v) : Float     { return untyped __call__("asin", v); }
	public static function acos(v) : Float     { return untyped __call__("acos", v); }
	public static function pow(b,e) : Float    { return untyped __call__("pow", b, e); }
	public static function random() : Float    { return untyped __call__("mt_rand") / __call__("mt_getrandmax"); }
	public static function isNaN(f) : Bool     { return untyped __call__("is_nan", f); }
	public static function isFinite(f) : Bool  { return untyped __call__("is_finite", f); }

	static function __init__() {
	 	PI = untyped __php__("M_PI");
	 	NaN = untyped __php__("acos(1.01)");
	 	NEGATIVE_INFINITY = untyped __php__("log(0)");
	 	POSITIVE_INFINITY = -NEGATIVE_INFINITY;
	}

}



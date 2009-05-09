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
package php;

class PhpMath__
{
	public static var PI;
	public static var NaN;
	public static var POSITIVE_INFINITY;
	public static var NEGATIVE_INFINITY;

	public static function abs(v)      { return untyped __call__("abs", v); }
	public static function min(a,b)    { return untyped __call__("min", a, b); }
	public static function max(a,b)    { return untyped __call__("max", a, b); }
	public static function sin(v)      { return untyped __call__("sin", v); }
	public static function cos(v)      { return untyped __call__("cos", v); }
	public static function atan2(y,x)  { return untyped __call__("atan2", y, x); }
	public static function tan(v)      { return untyped __call__("tan", v); }
	public static function exp(v)      { return untyped __call__("exp", v); }
	public static function log(v)      { return untyped __call__("log", v); }
	public static function sqrt(v)     { return untyped __call__("sqrt", v); }
	public static function round(v)    { return untyped __call__("round", v); }
	public static function floor(v)    { return untyped __call__("(int) floor", v); }
	public static function ceil(v)     { return untyped __call__("ceil", v); }
	public static function atan(v)     { return untyped __call__("atan", v); }
	public static function asin(v)     { return untyped __call__("asin", v); }
	public static function acos(v)     { return untyped __call__("acos", v); }
	public static function pow(b,e)    { return untyped __call__("pow", b, e); }
	public static function random()    { return untyped __call__("mt_rand") / __call__("mt_getrandmax"); }
	public static function isNaN(f)    { return untyped __call__("is_nan", f); }
	public static function isFinite(f) { return untyped __call__("is_finite", f); }

	static function __init__() {
	 	PI = untyped __php__("M_PI");
	 	NaN = untyped __php__("acos(1.01)");
	 	NEGATIVE_INFINITY = untyped __php__("log(0)");
	 	POSITIVE_INFINITY = -NEGATIVE_INFINITY;
	}

}



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

// Can't enable @:coreApi because some fields are now inline getters
// @:coreApi
@:keepInit
extern class Math
{
	static var PI(default,null) : Float;

	static var NEGATIVE_INFINITY(get, null) : Float;
	@:pure private static inline function get_NEGATIVE_INFINITY () : Float {
		return -(untyped __js__("Infinity"));
	}

	static var POSITIVE_INFINITY(get,null) : Float;
	@:pure private static inline function get_POSITIVE_INFINITY () : Float {
		return (untyped __js__("Infinity"));
	}

	static var NaN(get, null) : Float;
	@:pure private static inline function get_NaN () : Float {
		return (untyped __js__("NaN"));
	}

	@:pure static function abs(v:Float):Float;
	@:pure static function acos(v:Float):Float;
	@:pure static function asin(v:Float):Float;
	@:pure static function atan(v:Float):Float;
	@:pure static function atan2(y:Float, x:Float):Float;
	@:pure static function ceil(v:Float):Int;
	@:pure static function cos(v:Float):Float;
	@:pure static function exp(v:Float):Float;
	@:pure static function floor(v:Float):Int;
	@:pure static function log(v:Float):Float;
	@:pure static function max(a:Float, b:Float):Float;
	@:pure static function min(a:Float, b:Float):Float;
	@:pure static function pow(v:Float, exp:Float):Float;
	static function random() : Float;
	@:pure static function round(v:Float):Int;
	@:pure static function sin(v:Float):Float;
	@:pure static function sqrt(v:Float):Float;
	@:pure static function tan(v:Float):Float;

	@:pure static inline function ffloor( v : Float ) : Float {
		return floor(v);
	}

	@:pure static inline function fceil( v : Float ) : Float {
		return ceil(v);
	}

	@:pure static inline function fround( v : Float ) : Float {
		return round(v);
	}

	@:pure static inline function isFinite( f : Float ) : Bool {
		return (untyped __js__("isFinite"))(f);
	}

	@:pure static inline function isNaN( f : Float ) : Bool {
		return (untyped __js__("isNaN"))(f);
	}

	static function __init__() : Void {
		untyped __feature__("Type.resolveClass", $hxClasses["Math"] = Math);
	}
}

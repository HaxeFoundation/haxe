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
@:headerClassCode("\t\tstatic inline String string(String &s) { return s; }")
@:coreApi class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	@:keep public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static function isOfType(v:Dynamic, t:Dynamic):Bool {
		return untyped __global__.__instanceof(v, t);
	}

	@:keep public static function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		return Std.isOfType(value, c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	@:keep public static function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return inline downcast(value, c);
	}

	@:keep public static function string(s:Dynamic):String {
		return untyped s == null ? "null" : s.toString();
	}

	@:keep public static function int(x:Float):Int {
		return untyped __global__.__int__(x);
	}

	@:keep public static function parseInt(x:String):Null<Int> {
		return untyped __global__.__hxcpp_parse_int(x);
	}

	@:keep public static function parseFloat(x:String):Float {
		return untyped __global__.__hxcpp_parse_float(x);
	}

	@:keep public static function random(x:Int):Int {
		if (x <= 0)
			return 0;
		return untyped __global__.__hxcpp_irand(x);
	}
}

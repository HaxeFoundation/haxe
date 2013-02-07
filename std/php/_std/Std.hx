/*
 * Copyright (C)2005-2012 Haxe Foundation
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
@:coreApi class Std {

	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped untyped __call__("_hx_instanceof", v,t);
	}

	public static function string( s : Dynamic ) : String {
		return untyped __call__("_hx_string_rec", s, '');
	}

	public static function int( x : Float ) : Int {
		var i : Int = untyped __call__("fmod", x, 0x80000000) & 0xffffffff;
		if (untyped i & 0x80000000)
        	i = -((~i & 0xFFFFFFFF) + 1);
        return i;
	}

	public static function parseInt( x : String ) : Null<Int> {
		untyped if (!__call__("is_numeric", x)) {
			var matches = null;
			__call__('preg_match', '/^-?\\d+/', x, matches);
			return __call__("count", matches) == 0 ? null : __call__('intval', matches[0]);
		} else
			return x.substr(0, 2).toLowerCase() == "0x" ? __php__("(int) hexdec(substr($x, 2))") : __php__("intval($x)");
	}

	public static function parseFloat( x : String ) : Float {
		var v : Float = untyped __call__("floatval", x);
		untyped	if (v==0.0) {
			x=untyped __call__("rtrim", x);
			v=untyped __call__("floatval", x);
			if (v == 0.0 && !__call__("is_numeric", x)) v = untyped __call__("acos", 1.01);
		}
		return v;
	}

	public static function random( x : Int ) : Int {
		return untyped x <= 0 ? 0 : __call__("mt_rand", 0, x-1);
	}

}

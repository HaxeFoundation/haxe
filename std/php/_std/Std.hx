import php.Boot;

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
import php.Syntax;

@:keep
@:coreApi class Std {

	public static inline function is( v : Dynamic, t : Dynamic ) : Bool {
		return Boot.is(v, t);
	}

	public static inline function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return Boot.is(value, cast c) ? cast value : null;
	}

	public static function string( s : Dynamic ) : String {
		return Boot.stringify(s);
	}

	public static inline function int( x : Float ) : Int {
		return Syntax.int(x);
	}

	public static function parseInt( x : String ) : Null<Int> {
		if (Global.is_numeric(x)) {
			return Global.intval(x, 10);
		} else {
			x = Global.ltrim(x);
			var firstCharIndex = (x.charAt(0) == '-' ? 1 : 0);
			var firstCharCode = x.charCodeAt(firstCharIndex);
			if (!isDigitCode(firstCharCode)) {
				return null;
			}
			var secondChar = x.charAt(firstCharIndex + 1);
			if (secondChar == 'x' || secondChar == 'X') {
				return Global.intval(x, 0);
			} else {
				return Global.intval(x, 10);
			}
		}
	}

	public static function parseFloat( x : String ) : Float {
		var result = Global.floatval(x);
		if (result != 0) return result;

		x = Global.ltrim(x);
		var firstCharIndex = (x.charAt(0) == '-' ? 1 : 0);
		var charCode = x.charCodeAt(firstCharIndex);

		if (charCode == '.'.code) {
			charCode = x.charCodeAt(firstCharIndex + 1);
		}

		if (isDigitCode(charCode)) {
			return 0.0;
		} else {
			return Const.NAN;
		}
	}

	public static inline function random( x : Int ) : Int {
		return x <= 1 ? 0 : Global.mt_rand(0, x - 1);
	}

	static inline function isDigitCode( charCode:Null<Int> ) : Bool {
		return charCode != null && charCode >= '0'.code && charCode <= '9'.code;
	}

}

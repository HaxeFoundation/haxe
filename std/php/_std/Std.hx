import php.Boot;
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
import php.Global;
import php.Const;
import php.Syntax;

@:coreApi class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static inline function isOfType(v:Dynamic, t:Dynamic):Bool {
		return Boot.isOfType(v, t);
	}

	public static inline function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		return Boot.isOfType(value, cast c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	public static inline function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return Boot.isOfType(value, cast c) ? cast value : null;
	}

	public static function string(s:Dynamic):String {
		return Boot.stringify(s);
	}

	public static inline function int(x:Float):Int {
		return Syntax.int(x);
	}

	public static function parseInt(x:String):Null<Int> {
		x = Global.ltrim(x, " \t\n\x0b\x0c\r");
		final digitsOnly = Global.ltrim(x, "+-");
		if (Global.str_starts_with(digitsOnly, '0x') || Global.str_starts_with(digitsOnly, '0X')) {
			final val = Global.intval(x, 16); // hexadecimal
			// if the value was 0, ensure there is only a maximum of one + or - sign
			if (val == 0 && digitsOnly.length + 1 < x.length)
				return null;
			return val;
		}

		switch Global.stripos(x, 'e') {
			case false:
			case ePos: x = Global.substr(x, 0, ePos);
		}

		final val = Global.intval(x, 10);
		// if the value was 0, make sure it wasn't because the string had no valid digits
		// last check ensures there is only a maximum of one + or - sign
		if (val == 0 && (Global.strspn(digitsOnly, "0123456789", 0, 1) == 0 || digitsOnly.length + 1 < x.length))
			return null;
		return val;
	}

	public static function parseFloat(x:String):Float {
		var result = Global.floatval(x);
		if (result != 0)
			return result;

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

	public static inline function random(x:Int):Int {
		return x <= 1 ? 0 : Global.mt_rand(0, x - 1);
	}

	static inline function isDigitCode(charCode:Null<Int>):Bool {
		return charCode != null && charCode >= '0'.code && charCode <= '9'.code;
	}
}

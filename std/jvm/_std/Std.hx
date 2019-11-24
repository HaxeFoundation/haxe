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

import jvm.Jvm;

@:coreApi
class Std {
	public static function is(v:Dynamic, t:Dynamic):Bool {
		if (v == null || t == null) {
			return false;
		}
		var clt:java.lang.Class<Dynamic> = cast t;
		if (clt == null) {
			return false;
		}
		if (clt == cast java.lang.Object) {
			return true;
		}
		clt = Jvm.getWrapperClass(clt);
		if (clt == cast java.lang.Double.DoubleClass) {
			// Haxe semantics: any number is assignable to Float
			clt = cast java.lang.Number;
		} else if (clt == cast java.lang.Integer.IntegerClass) {
			if (!Jvm.instanceof(v, java.lang.Number)) {
				return false;
			}
			var n = (cast v : java.lang.Number);
			// Haxe semantics: 2.0 is an integer...
			return n.doubleValue() == n.intValue();
		}
		return clt.isAssignableFrom((v : java.lang.Object).getClass());
	}

	public static function string(s:Dynamic):String {
		return jvm.Jvm.toString(s);
	}

	public static function int(x:Float):Int {
		return cast x;
	}

	static var integerFormatter = java.text.NumberFormat.getIntegerInstance(java.util.Locale.US);
	static var doubleFormatter = {
		var fmt = new java.text.DecimalFormat();
		fmt.setParseBigDecimal(true);
		fmt.setDecimalFormatSymbols(new java.text.DecimalFormatSymbols(java.util.Locale.US));
		fmt;
	};

	public static function parseInt(x:String):Null<Int> {
		try {
			x = StringTools.trim(x);
			var signChars = switch (cast x : java.NativeString).codePointAt(0) {
				case '-'.code | '+'.code: 1;
				case _: 0;
			}
			if (x.length < 2 + signChars) {
				return integerFormatter.parse(x).intValue();
			}
			switch ((cast x : java.NativeString).codePointAt(1 + signChars)) {
				case 'x'.code | 'X'.code:
					return java.lang.Integer.decode(x).intValue();
				case _:
					return integerFormatter.parse(x).intValue();
			}
		} catch (_:Dynamic) {
			return null;
		}
	}

	public static function parseFloat(x:String):Float {
		try {
			x = StringTools.trim(x);
			x = x.split("+").join(""); // TODO: stupid
			return doubleFormatter.parse(x.toUpperCase()).doubleValue();
		} catch (_:Dynamic) {
			return Math.NaN;
		}
	}

	inline public static function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		return Std.is(value, c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	inline public static function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return downcast(value, c);
	}

	public static function random(x:Int):Int {
		if (x <= 0) {
			return 0;
		}
		return Std.int(Math.random() * x);
	}
}

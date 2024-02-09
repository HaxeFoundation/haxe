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

import js.Boot;
import js.Syntax;

@:keepInit
@:coreApi class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static inline function isOfType(v:Dynamic, t:Dynamic):Bool {
		return @:privateAccess js.Boot.__instanceof(v, t);
	}

	public static inline function downcast<T:{}, S:T>(value:T, c:Class<S>):S @:privateAccess {
		return if (js.Boot.__downcastCheck(value, c)) cast value else null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	public static inline function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return downcast(value, c);
	}

	@:pure
	public static function string(s:Dynamic):String {
		return @:privateAccess js.Boot.__string_rec(s, "");
	}

	public static inline function int(x:Float):Int {
		return (cast x) | 0;
	}

	@:pure
	public static function parseInt(x:String):Null<Int> {
		#if (js_es >= 5)
		final v = js.Lib.parseInt(x);
		#else
		// before ES5, octal was supported in some implementations, so we need to explicitly use base 10 or 16
		if (x == null)
			return null;
		var v:Float = Math.NaN;
		for (i => c in StringTools.keyValueIterator(x)) {
			if ((c <= 8 || c >= 14) && !(c == ' '.code || c == '-'.code || c == '+'.code)) {
				final nc = js.Syntax.code("{0}[{1}]", x, i + 1);
				v = js.Lib.parseInt(x, c == '0'.code && (nc == "x" || nc == "X") ? 16 : 10);
				break;
			}
		}
		#end
		if (Math.isNaN(v))
			return null;
		return cast v;
	}

	public static inline function parseFloat(x:String):Float {
		return js.Syntax.code("parseFloat({0})", x);
	}

	public static function random(x:Int):Int {
		return x <= 0 ? 0 : Math.floor(Math.random() * x);
	}

	static function __init__():Void untyped {
		__feature__("js.Boot.getClass",
			Object.defineProperty(String.prototype, "__class__",
				{value: __feature__("Type.resolveClass", $hxClasses["String"] = String, String), enumerable: false, writable: true}));
		__feature__("js.Boot.isClass", String.__name__ = "String");
		__feature__("Type.resolveClass", $hxClasses["Array"] = Array);
		__feature__("js.Boot.isClass", Array.__name__ = "Array");
		__feature__("Date.*", {
			__feature__("js.Boot.getClass",
				js.Syntax.code('Date')
					.prototype.__class__ = __feature__("Type.resolveClass", $hxClasses["Date"] = js.Syntax.code('Date'), js.Syntax.code('Date')));
			__feature__("js.Boot.isClass", js.Syntax.code('Date').__name__ = "Date");
		});
		__feature__("Int.*", js.Syntax.code('var Int = { };'));
		__feature__("Dynamic.*", js.Syntax.code('var Dynamic = { };'));
		__feature__("Float.*", js.Syntax.code('var Float = Number'));
		__feature__("Bool.*", js.Syntax.code('var Bool = Boolean'));
		__feature__("Class.*", js.Syntax.code('var Class = { };'));
		__feature__("Enum.*", js.Syntax.code('var Enum = { };'));
		#if (js_es < 5)
		__feature__("Array.map", if (Array.prototype.map == null) Array.prototype.map = function(f) {
			var a = [];
			for (i in 0...__this__.length)
				a[i] = f(__this__[i]);
			return a;
		});
		__feature__("Array.filter", if (Array.prototype.filter == null) Array.prototype.filter = function(f) {
			var a = [];
			for (i in 0...__this__.length) {
				var e = __this__[i];
				if (f(e))
					a.push(e);
			}
			return a;
		});
		#end
	}
}

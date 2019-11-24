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
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return @:privateAccess js.Boot.__instanceof(v, t);
	}

	public static inline function downcast<T:{}, S:T>(value:T, c:Class<S>):S@:privateAccess {
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
		if(x != null) {
			for(i in 0...x.length) {
				var c = StringTools.fastCodeAt(x, i);
				if(c <= 8 || (c >= 14 && c != ' '.code && c != '-'.code)) {
					var v:Int = Syntax.code('parseInt({0}, ({0}[{1}]=="x" || {0}[{1}]=="X") ? 16 : 10)', x, i + 1);
					return Math.isNaN(v) ? null : v;
				}
			}
		}
		return null;
	}

	public static inline function parseFloat(x:String):Float {
		return untyped __js__("parseFloat")(x);
	}

	public static function random(x:Int):Int {
		return x <= 0 ? 0 : Math.floor(Math.random() * x);
	}

	static function __init__():Void
		untyped {
			__feature__("js.Boot.getClass", String.prototype.__class__ = __feature__("Type.resolveClass", $hxClasses["String"] = String, String));
			__feature__("js.Boot.isClass", String.__name__ = __feature__("Type.getClassName", "String", true));
			__feature__("Type.resolveClass", $hxClasses["Array"] = Array);
			__feature__("js.Boot.isClass", Array.__name__ = __feature__("Type.getClassName", "Array", true));
			__feature__("Date.*", {
				__feature__("js.Boot.getClass",
					__js__('Date').prototype.__class__ = __feature__("Type.resolveClass", $hxClasses["Date"] = __js__('Date'), __js__('Date')));
				__feature__("js.Boot.isClass", __js__('Date').__name__ = "Date");
			});
			__feature__("Int.*", __js__('var Int = { };'));
			__feature__("Dynamic.*", __js__('var Dynamic = { };'));
			__feature__("Float.*", __js__('var Float = Number'));
			__feature__("Bool.*", __js__('var Bool = Boolean'));
			__feature__("Class.*", __js__('var Class = { };'));
			__feature__("Enum.*", __js__('var Enum = { };'));
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

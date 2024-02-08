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
@:coreApi class Reflect {
	@:pure
	public inline static function hasField(o:Dynamic, field:String):Bool {
		return js.lib.Object.prototype.hasOwnProperty.call(o, field);
	}

	@:pure
	public static function field(o:Dynamic, field:String):Dynamic {
		try
			return o[cast field]
		catch (e:Dynamic)
			return null;
	}

	public inline static function setField(o:Dynamic, field:String, value:Dynamic):Void {
		o[cast field] = value;
	}

	public static function getProperty(o:Dynamic, field:String):Dynamic
		untyped {
			var tmp;
			return if (o == null) __define_feature__("Reflect.getProperty",
				null) else if (o.__properties__ && (tmp = o.__properties__["get_" + field])) o[tmp]() else o[field];
		}

	public static function setProperty(o:Dynamic, field:String, value:Dynamic):Void
		untyped {
			var tmp;
			if (o.__properties__ && (tmp = o.__properties__["set_" + field]))
				o[tmp](value)
			else
				o[field] = __define_feature__("Reflect.setProperty", value);
		}

	public inline static function callMethod(o:Dynamic, func:haxe.Constraints.Function, args:Array<Dynamic>):Dynamic {
		return (cast func : js.lib.Function).apply(o, args);
	}

	public static function fields(o:Dynamic):Array<String> {
		var a = [];
		if (o != null)
			untyped {
				var hasOwnProperty = js.lib.Object.prototype.hasOwnProperty;
				js.Syntax.code("for( var f in o ) {");
				if (f != "__id__" && f != "hx__closures__" && hasOwnProperty.call(o, f))
					a.push(f);
				js.Syntax.code("}");
			}
		return a;
	}

	@:access(js.Boot)
	public static function isFunction(f:Dynamic):Bool {
		return js.Syntax.typeof(f) == "function" && !(js.Boot.isClass(f) || js.Boot.isEnum(f));
	}

	public static function compare<T>(a:T, b:T):Int {
		return (a == b) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static inline function compareMethods(f1:Dynamic, f2:Dynamic):Bool {
		return f1 == f2;
	}

	@:access(js.Boot)
	public static function isObject(v:Dynamic):Bool {
		if (v == null)
			return false;
		var t = js.Syntax.typeof(v);
		return (t == "string" || (t == "object" && v.__enum__ == null))
			|| (t == "function" && (js.Boot.isClass(v) || js.Boot.isEnum(v)) != null);
	}

	public static function isEnumValue(v:Dynamic):Bool {
		return v != null && v.__enum__ != null;
	}

	public static function deleteField(o:Dynamic, field:String):Bool {
		if (!hasField(o, field))
			return false;
		js.Syntax.delete(o, field);
		return true;
	}

	public static function copy<T>(o:Null<T>):Null<T> {
		if (o == null)
			return null;
		var o2:Dynamic = {};
		for (f in Reflect.fields(o))
			Reflect.setField(o2, f, Reflect.field(o, f));
		return o2;
	}

	@:overload(function(f:Array<Dynamic>->Void):Dynamic {})
	public static function makeVarArgs(f:Array<Dynamic>->Dynamic):Dynamic {
		return function() {
			var a = untyped Array.prototype.slice.call(js.Syntax.code("arguments"));
			return f(a);
		};
	}
}

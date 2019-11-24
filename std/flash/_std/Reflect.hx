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
	public static function hasField(o:Dynamic, field:String):Bool
		untyped {
			return o.hasOwnProperty(field);
		}

	public static function field(o:Dynamic, field:String):Dynamic
		untyped {
			return o != null && __in__(field, o) ? o[field] : null;
		}

	public inline static function setField(o:Dynamic, field:String, value:Dynamic):Void
		untyped {
			o[field] = value;
		}

	public static function getProperty(o:Dynamic, field:String):Dynamic
		untyped {
			if (o == null)
				return null;
			var getter = 'get_$field';
			if (__in__(getter, o)) {
				return o[getter]();
			}
			return __in__(field, o) ? o[field] : null;
		}

	public static function setProperty(o:Dynamic, field:String, value:Dynamic):Void
		untyped {
			var setter = 'set_$field';
			if (__in__(setter, o)) {
				o[setter](value);
			} else {
				o[field] = value;
			}
		}

	public inline static function callMethod(o:Dynamic, func:haxe.Constraints.Function, args:Array<Dynamic>):Dynamic
		untyped {
			return func.apply(o, args);
		}

	public static function fields(o:Dynamic):Array<String>
		untyped {
			if (o == null)
				return new Array();
			var i = 0;
			var a = [];
			while (untyped __has_next__(o, i)) {
				var prop = untyped __forin__(o, i);
				if (o.hasOwnProperty(prop))
					a.push(prop);
			}
			return a;
		}

	public static function isFunction(f:Dynamic):Bool
		untyped {
			return __typeof__(f) == "function";
		}

	public static function compare<T>(a:T, b:T):Int {
		var a:Dynamic = a;
		var b:Dynamic = b;
		return (a == b) ? 0 : ((a > b) ? 1 : -1);
	}

	public static function compareMethods(f1:Dynamic, f2:Dynamic):Bool {
		return f1 == f2; // VM-level closures
	}

	public static function isObject(v:Dynamic):Bool
		untyped {
			if (v == null)
				return false;
			var t = __typeof__(v);
			if (t == "object") {
				return !isEnumValue(v);
			}
			return (t == "string");
		}

	public static function isEnumValue(v:Dynamic):Bool {
		return try v.__enum__ == true catch (e:Dynamic) false;
	}

	public static function deleteField(o:Dynamic, field:String):Bool
		untyped {
			if (o.hasOwnProperty(field) != true)
				return false;
			__delete__(o, field);
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
		return function(__arguments__) {
			return f(__arguments__);
		};
	}
}

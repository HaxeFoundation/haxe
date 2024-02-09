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

package js;

import js.Syntax; // import it here so it's always available in the compiler

@:dox(hide)
class Boot {
	static inline function isClass(o:Dynamic):Bool {
		return untyped __define_feature__("js.Boot.isClass", o.__name__);
	}

	static inline function isInterface(o:Class<Dynamic>):Bool {
		return untyped __define_feature__("js.Boot.isInterface", o.__isInterface__);
	}

	static inline function isEnum(e:Dynamic):Bool {
		return untyped __define_feature__("js.Boot.isEnum", e.__ename__);
	}

	@:pure static function getClass(o:Null<Dynamic>):Null<Dynamic> {
		if (o == null) {
			return null;
		} else if (Std.isOfType(o, Array)) {
			return Array;
		} else {
			var cl = untyped __define_feature__("js.Boot.getClass", o.__class__);
			if (cl != null)
				return cl;
			var name = __nativeClassName(o);
			if (name != null)
				return __resolveNativeClass(name);
			return null;
		}
	}

	@:ifFeature("has_enum")
	private static function __string_rec(o, s:String) {
		untyped {
			if (o == null)
				return "null";
			if (s.length >= 5)
				return "<...>"; // too much deep recursion
			var t = js.Syntax.typeof(o);
			if (t == "function" && (isClass(o) || isEnum(o)))
				t = "object";
			switch (t) {
				case "object":
					#if !js_enums_as_arrays
					__feature__("has_enum", if (o.__enum__) {
						var e = $hxEnums[o.__enum__];
						var con = e.__constructs__[o._hx_index];
						var n = con._hx_name;
						if (o.__params__) {
							s += "\t";
							var params:Array<Any> = o.__params__();
							for (i in 0...params.length)
								params[i] = __string_rec(params[i], s);
							return n + "(" + params.join(",") + ")";
						} else {
							return n;
						}
					});
					#end
					if (js.Syntax.instanceof(o, Array)) {
						#if js_enums_as_arrays
						__feature__("has_enum", if (o.__enum__) {
							if (o.length == 2)
								return o[0];
							var str = o[0] + "(";
							s += "\t";
							for (i in 2...o.length) {
								if (i != 2)
									str += "," + __string_rec(o[i], s);
								else
									str += __string_rec(o[i], s);
							}
							return str + ")";
						});
						#end
						var str = "[";
						s += "\t";
						for (i in 0...o.length)
							str += (if (i > 0) "," else "") + __string_rec(o[i], s);
						str += "]";
						return str;
					}
					var tostr;
					try {
						tostr = untyped o.toString;
					} catch (e:Dynamic) {
						// strange error on IE
						return "???";
					}
					if (tostr != null && tostr != js.Syntax.code("Object.toString") && js.Syntax.typeof(tostr) == "function") {
						var s2 = o.toString();
						if (s2 != "[object Object]")
							return s2;
					}
					var str = "{\n";
					s += "\t";
					var hasp = (o.hasOwnProperty != null);
					var k:String = null;
					js.Syntax.code("for( {0} in {1} ) {", k, o);
					if (hasp && !o.hasOwnProperty(k))
						js.Syntax.code("continue");
					if (k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" || k == "__properties__")
						js.Syntax.code("continue");
					if (str.length != 2)
						str += ", \n";
					str += s + k + " : " + __string_rec(o[k], s);
					js.Syntax.code("}");
					s = s.substring(1);
					str += "\n" + s + "}";
					return str;
				case "function":
					return "<function>";
				case "string":
					return o;
				default:
					return String(o);
			}
		}
	}

	@:pure private static function __interfLoop(cc:Dynamic, cl:Dynamic) {
		if (cc == null)
			return false;
		if (cc == cl)
			return true;
		var intf:Dynamic = cc.__interfaces__;
		if (intf != null
			// ES6 classes inherit statics, so we want to avoid accessing inherited `__interfaces__`
			#if (js_es >= 6) && (cc.__super__ == null || cc.__super__.__interfaces__ != intf) #end
		) {
			for (i in 0...intf.length) {
				var i:Dynamic = intf[i];
				if (i == cl || __interfLoop(i, cl))
					return true;
			}
		}
		return __interfLoop(cc.__super__, cl);
	}

	@:pure private static function __instanceof(o:Dynamic, cl:Dynamic) {
		if (cl == null)
			return false;
		switch (cl) {
			case Int:
				return js.Syntax.typeof(o) == "number" && js.Syntax.strictEq(o | 0, o);
			case Float:
				return js.Syntax.typeof(o) == "number";
			case Bool:
				return js.Syntax.typeof(o) == "boolean";
			case String:
				return js.Syntax.typeof(o) == "string";
			case Array:
				return js.Syntax.instanceof(o, Array) #if js_enums_as_arrays && o.__enum__ == null #end;
			case Dynamic:
				return o != null;
			default:
				if (o != null) {
					// Check if o is an instance of a Haxe class or a native JS object
					if (js.Syntax.typeof(cl) == "function") {
						if (__downcastCheck(o, cl))
							return true;
					} else if (js.Syntax.typeof(cl) == "object" && __isNativeObj(cl)) {
						if (js.Syntax.instanceof(o, cl))
							return true;
					}
				} else {
					return false;
				}
				// do not use isClass/isEnum here
				untyped __feature__("Class.*", if (cl == Class && o.__name__ != null) return true);
				untyped __feature__("Enum.*", if (cl == Enum && o.__ename__ != null) return true);
				#if js_enums_as_arrays
				return o.__enum__ == cl;
				#else
				return untyped __feature__(
					"has_enum",
					if (o.__enum__ != null) ($hxEnums[o.__enum__]) == cl else false,
					false
				);
				#end
		}
	}

	static function __downcastCheck(o:Dynamic, cl:Class<Dynamic>):Bool {
		return js.Syntax.instanceof(o, cl) || (isInterface(cl) && inline __implements(o, cl));
	}

	static function __implements(o:Dynamic, iface:Class<Dynamic>):Bool {
		return __interfLoop(getClass(o), iface);
	}

	@:ifFeature("typed_cast") private static function __cast(o:Dynamic, t:Dynamic) {
		if (o == null || __instanceof(o, t))
			return o;
		else
			throw "Cannot cast " + Std.string(o) + " to " + Std.string(t);
	}

	static var __toStr:js.lib.Function;

	static function __init__() {
		Boot.__toStr = (cast {}).toString;
	}

	// get native JS [[Class]]
	static function __nativeClassName(o:Dynamic):String {
		var name:String = __toStr.call(o).slice(8, -1);
		// exclude general Object and Function
		// also exclude Math and JSON, because instanceof cannot be called on them
		if (name == "Object" || name == "Function" || name == "Math" || name == "JSON")
			return null;
		return name;
	}

	// check for usable native JS object
	static function __isNativeObj(o:Dynamic):Bool {
		return __nativeClassName(o) != null;
	}

	// resolve native JS class in the global scope:
	static function __resolveNativeClass(name:String) {
		return js.Lib.global[cast name];
	}
}

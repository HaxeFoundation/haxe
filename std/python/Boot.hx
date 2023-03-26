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

package python;

import python.internal.MethodClosure;
import python.internal.ArrayImpl;
import python.internal.Internal;
import python.internal.StringImpl;
import python.internal.EnumImpl;
import python.internal.HxOverrides;
import python.internal.AnonObject;
import python.internal.UBuiltins;
import python.lib.Inspect;
import python.Syntax;

@:dox(hide)
class Boot {
	static var keywords:Set<String> = new Set([
		"and", "del", "from", "not", "with", "as", "elif", "global", "or", "yield", "assert", "else", "if", "pass", "None", "break", "except", "import",
		"raise", "True", "class", "exec", "in", "return", "False", "continue", "finally", "is", "try", "def", "for", "lambda", "while",
	]);

	inline static function arrayJoin<T>(x:Array<T>, sep:String):String {
		return Syntax.field(sep, "join")(Syntax.code("[{0}(x1,'') for x1 in {1}]", python.Boot.toString1, x));
	}

	inline static function safeJoin(x:Array<String>, sep:String):String {
		return Syntax.field(sep, "join")(Syntax.code("[x1 for x1 in {0}]", x));
	}

	inline static function isPyBool(o:Dynamic):Bool {
		return UBuiltins.isinstance(o, UBuiltins.bool);
	}

	inline static function isPyInt(o:Dynamic):Bool {
		// for historic reasons bool extends int
		return UBuiltins.isinstance(o, UBuiltins.int) && !isPyBool(o);
	}

	inline static function isPyFloat(o:Dynamic):Bool {
		return UBuiltins.isinstance(o, UBuiltins.float);
	}

	static inline function isClass(o:Dynamic):Bool {
		return o != null && (o == String || Inspect.isclass(o));
	}

	static inline function isAnonObject(o:Dynamic) {
		return UBuiltins.isinstance(o, AnonObject);
	}

	@:ifFeature("add_dynamic") private static function _add_dynamic(a:Dynamic, b:Dynamic):Dynamic {
		if (UBuiltins.isinstance(a, String) && UBuiltins.isinstance(b, String)) {
			return Syntax.binop(a, "+", b);
		}
		if (UBuiltins.isinstance(a, String) || UBuiltins.isinstance(b, String)) {
			return Syntax.binop(toString1(a, ""), "+", toString1(b, ""));
		}
		return Syntax.binop(a, "+", b);
	}

	static inline function toString(o:Dynamic) {
		return toString1(o, "");
	}

	private static function toString1(o:Dynamic, s:String):String {
		if (o == null)
			return "null";

		if (isString(o))
			return o;

		if (s == null)
			s = "";
		if (s.length >= 5)
			return "<...>"; // too much deep recursion

		if (isPyBool(o)) {
			if ((o : Bool))
				return "true"
			else
				return "false";
		}
		if (isPyInt(o)) {
			return UBuiltins.str(o);
		}
		// 1.0 should be printed as 1
		if (isPyFloat(o)) {
			try {
				if ((o : Float) == UBuiltins.int(o)) {
					return UBuiltins.str(Math.round(o));
				} else {
					return UBuiltins.str(o);
				}
			} catch (e:Dynamic) {
				return UBuiltins.str(o);
			}
		}

		if (isArray(o)) {
			var o1:Array<Dynamic> = o;

			var l = o1.length;

			var st = "[";
			s += "\t";
			for (i in 0...l) {
				var prefix = "";
				if (i > 0) {
					prefix = ",";
				}
				st += prefix + toString1(o1[i], s);
			}
			st += "]";
			return st;
		}

		try {
			if (UBuiltins.hasattr(o, "toString"))
				return Syntax.callField(o, "toString");
		} catch (e:Dynamic) {}

		if (UBuiltins.hasattr(o, "__class__")) {
			if (isAnonObject(o)) {
				var toStr = null;
				try {
					var fields = fields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(simpleField(o, f), s + "\t")}'];
					toStr = "{ " + safeJoin(fieldsStr, ", ") + " }";
				} catch (e:Dynamic) {
					return "{ ... }";
				}

				if (toStr == null) {
					return "{ ... }";
				} else {
					return toStr;
				}
			}
			if (UBuiltins.isinstance(o, Enum)) {
				var o:EnumImpl = (o : EnumImpl);

				var l = UBuiltins.len(o.params);
				var hasParams = l > 0;
				if (hasParams) {
					var paramsStr = "";
					for (i in 0...l) {
						var prefix = "";
						if (i > 0) {
							prefix = ",";
						}
						paramsStr += prefix + toString1(o.params[i], s);
					}
					return o.tag + "(" + paramsStr + ")";
				} else {
					return o.tag;
				}
			}

			if (Internal.hasClassName(o)) {
				if (Syntax.field(Syntax.field(o, "__class__"), "__name__") != "type") {
					var fields = getInstanceFields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(simpleField(o, f), s + "\t")}'];

					var toStr = (Internal.fieldClassName(o) : String) + "( " + safeJoin(fieldsStr, ", ") + " )";
					return toStr;
				} else {
					var fields = getClassFields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(simpleField(o, f), s + "\t")}'];
					var toStr = "#" + (Internal.fieldClassName(o) : String) + "( " + safeJoin(fieldsStr, ", ") + " )";
					return toStr;
				}
			}

			if (isMetaType(o, String)) {
				return "#String";
			}

			if (isMetaType(o, Array)) {
				return "#Array";
			}

			if (UBuiltins.callable(o)) {
				return "function";
			}
			try {
				if (UBuiltins.hasattr(o, "__repr__")) {
					return Syntax.callField(o, "__repr__");
				}
			} catch (e:Dynamic) {}

			if (UBuiltins.hasattr(o, "__str__")) {
				return Syntax.callField(o, "__str__", []);
			}

			if (UBuiltins.hasattr(o, "__name__")) {
				return Syntax.field(o, "__name__");
			}
			return "???";
		} else {
			return UBuiltins.str(o);
		}
	}

	static inline function isMetaType(v:Dynamic, t:Dynamic):Bool {
		return Syntax.binop(Syntax.binop(Syntax.call(UBuiltins.type, [v]), "==", UBuiltins.type), "and", Syntax.binop(v, "==", t));
	}

	@:analyzer(no_local_dce)
	static function fields(o:Dynamic) {
		var a = [];
		if (o != null) {
			if (Internal.hasFields(o)) {
				var fields = Internal.fieldFields(o);
				if (fields != null) {
					return (fields : Array<String>).copy();
				}
			}
			if (isAnonObject(o)) {
				var d = Syntax.field(o, "__dict__");
				var keys = Syntax.callField(d, "keys");
				var handler = unhandleKeywords;

				Syntax.code("for k in keys:");
				Syntax.code("    if (k != '_hx_disable_getattr'):");
				Syntax.code("        a.append(handler(k))");
			} else if (UBuiltins.hasattr(o, "__dict__")) {
				var a = [];
				var d = Syntax.field(o, "__dict__");
				var keys1 = Syntax.callField(d, "keys");
				Syntax.code("for k in keys1:");
				Syntax.code("    a.append(k)");
			}
		}
		return a;
	}

	static inline function isString(o:Dynamic):Bool {
		return UBuiltins.isinstance(o, UBuiltins.str);
	}

	static inline function isArray(o:Dynamic):Bool {
		return UBuiltins.isinstance(o, UBuiltins.list);
	}

	static function simpleField(o:Dynamic, field:String):Dynamic {
		if (field == null)
			return null;

		var field = handleKeywords(field);
		return if (UBuiltins.hasattr(o, field)) UBuiltins.getattr(o, field) else null;
	}

	@:ifFeature("closure_Array", "closure_String")
	static inline function createClosure(obj:Dynamic, func:Dynamic):Dynamic {
		return new MethodClosure(obj, func);
	}

	static function hasField(o:Dynamic, field:String):Bool {
		if (isAnonObject(o)) {
			return Syntax.code('{0}._hx_hasattr({1})', o, field);
		}
		return UBuiltins.hasattr(o, handleKeywords(field));
	}

	static function field(o:Dynamic, field:String):Dynamic {
		if (field == null)
			return null;

		inline function def() {
			var field = handleKeywords(field);
			return if (UBuiltins.hasattr(o, field)) UBuiltins.getattr(o, field) else null;
		}

		return if (isString(o)) {
			switch (field) {
				case "length":
					StringImpl.get_length(o);
				case "toLowerCase":
					createClosure(o, StringImpl.toLowerCase);
				case "toUpperCase":
					createClosure(o, StringImpl.toUpperCase);
				case "charAt":
					createClosure(o, StringImpl.charAt);
				case "charCodeAt":
					createClosure(o, StringImpl.charCodeAt);
				case "indexOf":
					createClosure(o, StringImpl.indexOf);
				case "lastIndexOf":
					createClosure(o, StringImpl.lastIndexOf);
				case "split":
					createClosure(o, StringImpl.split);
				case "substr":
					createClosure(o, StringImpl.substr);
				case "substring":
					createClosure(o, StringImpl.substring);
				case "toString":
					createClosure(o, StringImpl.toString);
				default:
					def();
			}
		} else if (isArray(o)) {
			switch (field) {
				case "length":
					ArrayImpl.get_length(o);
				case "map":
					createClosure(o, ArrayImpl.map);
				case "filter":
					createClosure(o, ArrayImpl.filter);
				case "concat":
					createClosure(o, ArrayImpl.concat);
				case "copy":
					createClosure(o, ArrayImpl.copy);
				case "iterator":
					createClosure(o, ArrayImpl.iterator);
				case "keyValueIterator":
					createClosure(o, ArrayImpl.keyValueIterator);
				case "insert":
					createClosure(o, ArrayImpl.insert);
				case "join":
					createClosure(o, ArrayImpl.join);
				case "toString":
					createClosure(o, ArrayImpl.toString);
				case "pop":
					createClosure(o, ArrayImpl.pop);
				case "push":
					createClosure(o, ArrayImpl.push);
				case "unshift":
					createClosure(o, ArrayImpl.unshift);
				case "indexOf":
					createClosure(o, ArrayImpl.indexOf);
				case "lastIndexOf":
					createClosure(o, ArrayImpl.lastIndexOf);
				case "contains":
					createClosure(o, ArrayImpl.contains);
				case "remove":
					createClosure(o, ArrayImpl.remove);
				case "reverse":
					createClosure(o, ArrayImpl.reverse);
				case "shift":
					createClosure(o, ArrayImpl.shift);
				case "slice":
					createClosure(o, ArrayImpl.slice);
				case "sort":
					createClosure(o, ArrayImpl.sort);
				case "splice":
					createClosure(o, ArrayImpl.splice);
				default:
					def();
			}
		} else {
			def();
		}
	}

	static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		var f = if (Internal.hasFields(c)) (Internal.fieldFields(c) : Array<String>).copy() else [];
		if (Internal.hasMethods(c))
			f = f.concat(Internal.fieldMethods(c));

		var sc = getSuperClass(c);

		if (sc == null) {
			return f;
		} else {
			var scArr = getInstanceFields(sc);
			var scMap = new Set(scArr);
			for (f1 in f) {
				if (!scMap.has(f1)) {
					scArr.push(f1);
				}
			}

			return scArr;
		}
	}

	static function getSuperClass(c:Class<Dynamic>):Class<Dynamic> {
		if (c == null)
			return null;

		try {
			if (Internal.hasSuper(c)) {
				return Internal.fieldSuper(c);
			}
			return null;
		} catch (e:Dynamic) {}
		return null;
	}

	static function getClassFields(c:Class<Dynamic>):Array<String> {
		if (Internal.hasStatics(c)) {
			var x:Array<String> = Internal.fieldStatics(c);
			return x.copy();
		} else {
			return [];
		}
	}

	static inline function unsafeFastCodeAt(s:String, index:Int) {
		return UBuiltins.ord(python.Syntax.arrayAccess(s, index));
	}

	static inline function handleKeywords(name:String):String {
		return if (keywords.has(name)) {
			Internal.getPrefixed(name);
		} else if (name.length > 2
			&& unsafeFastCodeAt(name, 0) == "_".code
			&& unsafeFastCodeAt(name, 1) == "_".code
			&& unsafeFastCodeAt(name, name.length - 1) != "_".code) {
			Internal.getPrefixed(name);
		} else
			name;
	}

	static var prefixLength = Internal.prefix().length;

	static function unhandleKeywords(name:String):String {
		if (name.substr(0, prefixLength) == Internal.prefix()) {
			var real = name.substr(prefixLength);
			if (keywords.has(real))
				return real;
		}
		return name;
	}

	static inline function implementsInterface(value:Dynamic, cls:Class<Dynamic>):Bool {
		function loop(intf) {
			var f:Array<Dynamic> = if (Internal.hasInterfaces(intf)) Internal.fieldInterfaces(intf) else [];
			if (f != null) {
				for (i in f) {
					if (i == cls) {
						return true;
					} else {
						var l = loop(i);
						if (l) {
							return true;
						}
					}
				}
				return false;
			} else {
				return false;
			}
		}
		var currentClass = Syntax.field(value, "__class__");
		var result = false;
		while (currentClass != null) {
			if (loop(currentClass)) {
				result = true;
				break;
			}
			currentClass = getSuperClass(currentClass);
		}
		return result;
	}
}

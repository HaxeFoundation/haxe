package python;

import python.internal.ArrayImpl;
import python.internal.Internal;
import python.internal.StringImpl;
import python.internal.EnumImpl;
import python.internal.HxOverrides;
import python.internal.HxException;
import python.internal.AnonObject;
import python.lib.Builtin;
import python.lib.Inspect;
import python.lib.Set;

import python.Syntax;

typedef HxClassBase = {
    _hx_class:Dynamic,
    _hx_class_name:String
}

private typedef HxAbstract = {
    >HxClassBase,
}

private typedef HxEnum = {
    >HxClassBase,
    _hx_constructs:Array<String>
}

private typedef HxClass = {
    >HxClassBase,
    _hx_fields:Array<String>,
    _hx_props:Array<String>,
    _hx_methods:Array<String>,
    _hx_statics:Array<String>,
    _hx_interfaces:Array<HxClassBase>,
    _hx_super:HxClass
}

@:keep
@:nativeGen
@:native("_hx_ClassRegistry")
private class ClassRegistry extends python.lib.Dict<String, HxClassBase> {
    function _register(cls:HxClassBase, name:String):Void {
        cls._hx_class = cls;
        cls._hx_class_name = name;
        set(name, cls);
    }

    function registerAbstract(name:String):HxAbstract->HxAbstract {
        function wrapper(cls:HxAbstract):HxAbstract {
            _register(cls, name);
            return cls;
        }
        return wrapper;
    }

    function registerEnum(name:String, constructs:Array<String>):HxEnum->HxEnum {
        function wrapper(cls:HxEnum):HxEnum {
            _register(cls, name);
            cls._hx_constructs = constructs;
            return cls;
        }
        return wrapper;
    }

    function registerClass(name:String, ?fields:Array<String>, ?props:Array<String>, ?methods:Array<String>, ?statics:Array<String>, ?interfaces:Array<HxClassBase>, ?superClass:HxClass):HxClass->HxClass {
        if (fields == null) fields = [];
        if (props == null) props = [];
        if (methods == null) methods = [];
        if (statics == null) statics = [];
        if (interfaces == null) interfaces = [];
        function wrapper(cls:HxClass):HxClass {
            _register(cls, name);
            cls._hx_fields = fields;
            cls._hx_props = props;
            cls._hx_methods = methods;
            cls._hx_statics = statics;
            cls._hx_interfaces = interfaces;
            if (superClass != null)
                cls._hx_super = superClass;
            return cls;
        }
        return wrapper;
    }
}

@:preCode("_hx_classes = _hx_ClassRegistry()")
@:keep class Boot {

	static var keywords:Set<String> = new Set(
	[
		"and",      "del",      "from",     "not",      "while",
		"as",       "elif",     "global",   "or",       "with",
		"assert",   "else",     "if",       "pass",     "yield",
		"break",    "except",   "import",   "print",    "float",
		"class",    "exec",     "in",       "raise",
		"continue", "finally",  "is",       "return",
		"def",      "for",      "lambda",   "try",
		"None",     "list",     "True",     "False"
	]);

	inline static function arrayJoin <T>(x:Array<T>, sep:String):String {
		return Syntax.field(sep, "join")(Syntax.pythonCode("[{0}(x1,'') for x1 in {1}]", python.Boot.toString1, x));
	}

	inline static function isPyBool(o:Dynamic):Bool {
		return Builtin.isinstance(o, Builtin.bool);
	}

	inline static function isPyInt(o:Dynamic):Bool {
		return Builtin.isinstance(o, Builtin.int);
	}

	inline static function isPyFloat(o:Dynamic):Bool {
		return Builtin.isinstance(o, Builtin.float);
	}

	static inline function isClass(o:Dynamic) : Bool {
		return o != null && (o == String || Inspect.isclass(o));
	}

	static inline function isAnonObject(o:Dynamic) {
		return Builtin.isinstance(o, AnonObject);
	}

	private static function _add_dynamic(a:Dynamic,b:Dynamic):Dynamic {
		if (Builtin.isinstance(a, String) || Builtin.isinstance(b, String)) {
			return Syntax.binop(toString1(a,""), "+", toString1(b,""));
		}
		return Syntax.binop(a, "+", b);
	}

	static inline function toString (o:Dynamic) {
		return toString1(o, "");
	}

	private static function toString1(o:Dynamic,s:String):String {

		if( o == null ) return "null";

		if (isString(o)) return o;

		if (s == null) s = "";
		if( s.length >= 5 ) return "<...>"; // too much deep recursion

		if (isPyBool(o)) {
			if ((o:Bool)) return "true" else return "false";
		}
		if (isPyInt(o)) {
			return Builtin.str(o);
		}
		// 1.0 should be printed as 1
		if (isPyFloat(o)) {
			try {
				if ( (o:Float) == Builtin.int(o)) {
					return Builtin.str(Math.round(o));
				} else {
					return Builtin.str(o);
				}
			} catch (e:Dynamic) {
				return Builtin.str(o);
			}
		}

		if (isArray(o))
		{
			var o1:Array<Dynamic> = o;
			var l = o1.length;

			var st = "[";
			s += "\t";
			for( i in 0...l ) {
				var prefix = "";
				if (i > 0) {
					prefix = ",";
				}
				st += prefix + toString1(o1[i],s);
			}
			st += "]";
			return st;
		}

		try {
			if (Builtin.hasattr(o, "toString"))
				return Syntax.callField(o, "toString");
		} catch (e:Dynamic) {
		}

		if (Inspect.isfunction(o) || Inspect.ismethod(o)) return "<function>";

		if (Builtin.hasattr(o, "__class__"))
		{

			if (isAnonObject(o))
			{
				var toStr = null;
				try
				{
					var fields = fields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(field(o,f), s+"\t")}'];
					toStr = "{ " + arrayJoin(fieldsStr, ", ") + " }";
				}
				catch (e:Dynamic) {
					return "{ ... }";
				}

				if (toStr == null)
				{
					return "{ ... }";
				}
				else
				{
					return toStr;
				}

			}
			if (Builtin.isinstance(o, Enum)) {

				var o:EnumImpl = o;

				var l = Builtin.len(o.params);
				var hasParams = l > 0;
				if (hasParams) {
					var paramsStr = "";
					for (i in 0...l) {
						var prefix = "";
						if (i > 0) {
							prefix = ",";
						}
						paramsStr += prefix + toString1(o.params[i],s);
					}
					return o.tag + "(" + paramsStr + ")";
				} else {
					return o.tag;
				}
			}

			if (Internal.hasClassName(o)) {
				if (Syntax.field(Syntax.field(o, "__class__"), "__name__") != "type") {
					var fields = getInstanceFields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(field(o,f), s+"\t")}'];

					var toStr = Internal.fieldClassName(o) + "( " + arrayJoin(fieldsStr, ", ") + " )";
					return toStr;
				} else {
					var fields = getClassFields(o);
					var fieldsStr = [for (f in fields) '$f : ${toString1(field(o,f), s+"\t")}'];
					var toStr = "#" + Internal.fieldClassName(o) + "( " + arrayJoin(fieldsStr, ", ") + " )";
					return toStr;
				}
			}

			if (isMetaType(o,String)) {
				return "#String";
			}

			if (isMetaType(o,Array)) {
				return "#Array";
			}

			if (Builtin.callable(o)) {
				return "function";
			}
			try {
				if (Builtin.hasattr(o, "__repr__")) {
					return Syntax.callField(o, "__repr__");
				}
			} catch (e:Dynamic) {}

			if (Builtin.hasattr(o, "__str__")) {
				return Syntax.callField(o, "__str__", []);
			}

			if (Builtin.hasattr(o, "__name__")) {
				return Syntax.field(o, "__name__");
			}
			return "???";
		} else {
			return Builtin.str(o);
		}
	}

	static inline function isMetaType(v:Dynamic, t:Dynamic):Bool {
		return python.Syntax.binop(v, "==", t);
	}

	@:analyzer(no_local_dce)
	static function fields (o:Dynamic) {
		var a = [];
		if (o != null) {
			if (Internal.hasFields(o)) {
				var fields:Array<String> = Internal.fieldFields(o);
				return fields.copy();
			}
			if (isAnonObject(o)) {

				var d = Syntax.field(o, "__dict__");
				var keys = Syntax.callField(d, "keys");
				var handler = unhandleKeywords;

				Syntax.pythonCode("for k in keys:");
				Syntax.pythonCode("	a.append(handler(k))");
			}
			else if (Builtin.hasattr(o, "__dict__")) {
				var a = [];
				var d = Syntax.field(o, "__dict__");
				var keys1  = Syntax.callField(d, "keys");
				Syntax.pythonCode("for k in keys1:");
				Syntax.pythonCode("	a.append(k)");

			}
		}
		return a;
	}

	static inline function isString (o:Dynamic):Bool {
		return Builtin.isinstance(o, Builtin.str);
	}

	static inline function isArray (o:Dynamic):Bool {
		return Builtin.isinstance(o, Builtin.list);
	}

	static function field( o : Dynamic, field : String ) : Dynamic {
		if (field == null) return null;

		switch (field) {
			case "length" if (isString(o)): return StringImpl.get_length(o);
			case "toLowerCase" if (isString(o)): return StringImpl.toLowerCase.bind(o);
			case "toUpperCase" if (isString(o)): return StringImpl.toUpperCase.bind(o);
			case "charAt" if (isString(o)): return StringImpl.charAt.bind(o);
			case "charCodeAt" if (isString(o)): return StringImpl.charCodeAt.bind(o);
			case "indexOf" if (isString(o)): return StringImpl.indexOf.bind(o);
			case "lastIndexOf" if (isString(o)): return StringImpl.lastIndexOf.bind(o);
			case "split" if (isString(o)): return StringImpl.split.bind(o);
			case "substr" if (isString(o)): return StringImpl.substr.bind(o);
			case "substring" if (isString(o)): return StringImpl.substring.bind(o);
			case "toString" if (isString(o)): return StringImpl.toString.bind(o);
			case "length" if (isArray(o)): return ArrayImpl.get_length(o);
			case "map" if (isArray(o)): return ArrayImpl.map.bind(o);
			case "filter" if (isArray(o)): return ArrayImpl.filter.bind(o);
			case "concat" if (isArray(o)): return ArrayImpl.concat.bind(o);
			case "copy" if (isArray(o)): return function () return ArrayImpl.copy(o);
			case "iterator" if (isArray(o)): return ArrayImpl.iterator.bind(o);
			case "insert" if (isArray(o)): return ArrayImpl.insert.bind(o);
			case "join" if (isArray(o)): return function (sep) return ArrayImpl.join(o, sep);
			case "toString" if (isArray(o)): return ArrayImpl.toString.bind(o);
			case "pop" if (isArray(o)): return ArrayImpl.pop.bind(o);
			case "push" if (isArray(o)): return ArrayImpl.push.bind(o);
			case "unshift" if (isArray(o)): return ArrayImpl.unshift.bind(o);
			case "indexOf" if (isArray(o)): return ArrayImpl.indexOf.bind(o);
			case "lastIndexOf" if (isArray(o)): return ArrayImpl.lastIndexOf.bind(o);
			case "remove" if (isArray(o)): return ArrayImpl.remove.bind(o);
			case "reverse" if (isArray(o)): return ArrayImpl.reverse.bind(o);
			case "shift" if (isArray(o)): return ArrayImpl.shift.bind(o);
			case "slice" if (isArray(o)): return ArrayImpl.slice.bind(o);
			case "sort" if (isArray(o)): return ArrayImpl.sort.bind(o);
			case "splice" if (isArray(o)): return ArrayImpl.splice.bind(o);
		}


		var field = handleKeywords(field);
		return if (Builtin.hasattr(o, field)) Builtin.getattr(o, field) else null;
	}


	static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		var f = if (Internal.hasFields(c)) {
			var x:Array<String> = Internal.fieldFields(c);
			var x2:Array<String> = Internal.fieldMethods(c);
			x.concat(x2);
		} else {
			[];
		}

		var sc = getSuperClass(c);

		if (sc == null) {
			return f;
		} else {
			var scArr = getInstanceFields(sc);
			var scMap = [for (f in scArr) f => f];
			var res = [];
			for (f1 in f) {
				if (!scMap.exists(f1)) {
					scArr.push(f1);
				}
			}

			return scArr;
		}
	}

	static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		if( c == null )
			return null;

		try {
			if (Internal.hasSuper(c)) {
				return Internal.fieldSuper(c);
			}
			return null;
		} catch (e:Dynamic) {

		}
		return null;

	}

	static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		if (Internal.hasStatics(c)) {
			var x:Array<String> = Internal.fieldStatics(c);
			return x.copy();
		} else {
			return [];
		}
	}



	static inline function unsafeFastCodeAt (s, index) {
		return Builtin.ord(python.Syntax.arrayAccess(s, index));
	}

	static inline function handleKeywords(name:String):String {
		return if (keywords.has(name)) {
			Internal.getPrefixed(name);
		} else if (name.length > 2 && unsafeFastCodeAt(name,0) == "_".code && unsafeFastCodeAt(name,1) == "_".code && unsafeFastCodeAt(name, name.length-1) != "_".code) {
			Internal.getPrefixed(name);
		}
		else name;
	}

	static var prefixLength = Internal.prefix().length;

	static function unhandleKeywords(name:String):String {
		if (name.substr(0,prefixLength) == Internal.prefix()) {
			var real = name.substr(prefixLength);
			if (keywords.has(real)) return real;
		}
		return name;
	}

}
/*
 * Copyright (C)2005-2012 Haxe Foundation
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

import python.internal.StringImpl;
import python.internal.ArrayImpl;

import python.lib.Builtin;
import python.lib.Inspect;
import python.lib.Types;

@:coreApi
class Reflect {

	static var keywords:Set<String> = new Set(
    [
        "and",       "del",       "from",      "not",       "while",
        "as",        "elif",      "global",    "or",        "with",
        "assert",    "else",      "if",        "pass",      "yield",
        "break",     "except",    "import",    "print",     "float",
        "class",     "exec",      "in",        "raise",
        "continue",  "finally",   "is",        "return",
        "def",       "for",       "lambda",    "try",
        "None",      "list"
    ]);

	static inline function handleKeywords(name:String):String
    {
        if (keywords.has(name)) {
            return "_hx_" + name;
        }
        return name;
    }

    static function unhandleKeywords(name:String):String
    {
    	if (name.substr(0,4) == "_hx_") {
    		var real = name.substr(4);
    		if (keywords.has(real)) return real;
    	}
    	return name;
    }

	public static function hasField( o : Dynamic, field : String ) : Bool
	{
		var field = handleKeywords(field);
		return Builtin.hasattr(o, field);
	}

	static inline function isString (o:Dynamic):Bool {
		return Builtin.isinstance(o, String);
	}
	static inline function isArray (o:Dynamic):Bool {
		return Builtin.isinstance(o, Array);
	}

	@:keep public static function field( o : Dynamic, field : String ) : Dynamic
	{
		if (field == null) return null;

		switch (field) {
			case "length" if (isString(o)): return StringImpl.get_length(o);
			case "length" if (isArray(o)): return ArrayImpl.get_length(o);

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

			case "map" if (isArray(o)): return ArrayImpl.map.bind(o);
			case "filter" if (isArray(o)): return ArrayImpl.filter.bind(o);
			case "concat" if (isArray(o)): return ArrayImpl.concat.bind(o);
			case "copy" if (isArray(o)): return ArrayImpl.copy.bind(o);
			case "iterator" if (isArray(o)): return ArrayImpl.iterator.bind(o);
			case "insert" if (isArray(o)): return ArrayImpl.insert.bind(o);
			case "join" if (isArray(o)): return ArrayImpl.join.bind(o);
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

	@:keep public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped
	{
		var field = handleKeywords(field);
		return __define_feature__("Reflect.setField",Builtin.setattr(o,field,value));
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic
	{
		var field = handleKeywords(field);
		var tmp = null;
		if (o == null) {
			return null;
		} else {
			tmp = Reflect.field(o, "get_" + field);
			if (tmp != null && Builtin.callable(tmp)) {
				return tmp();
			} else {
				return Reflect.field(o, field);
			}
		}
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {

		var field = handleKeywords(field);

		return if (Builtin.hasattr(o,"set_"+field)) {
			var tmp = Builtin.getattr(o,"set_"+field);
			tmp(value);
		}
		else Builtin.setattr(o,field, untyped __define_feature__("Reflect.setProperty",value));
	}

	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic
	{
		var args:VarArgs = args;
		return if (Builtin.callable(func)) func(untyped __python_varargs__(args)) else null;
	}

	public static function fields( o : Dynamic ) : Array<String>
	{
		var a = [];
		if (o != null)
		{
			if (Builtin.hasattr(o, "_hx_fields"))
			{

				var fields:Array<String> = o._hx_fields;
				return fields.copy();
			}
			if (Builtin.isinstance(o, untyped __python__("_hx_c._hx_AnonObject")))
			{

				var d:Dict<String, Dynamic> = Builtin.getattr(o, "__dict__");
				var keys  = d.keys();
				var handler = unhandleKeywords;
				untyped __python__("for k in keys:");
				untyped __python__("	a.append(handler(k))");

			}
			else if (Builtin.hasattr(o, "__dict__"))
			{
				var a = [];
				var d:Dict<String, Dynamic> = Builtin.getattr(o, "__dict__");
				var keys  = untyped d.keys();
				untyped __python__("for k in keys:");
				untyped __python__("	a.append(k)");

			}
		}
		return a;
	}

	public static function isFunction( f : Dynamic ) : Bool
	{
		return Inspect.isfunction(f) || Inspect.ismethod(f);
	}

	public static function compare<T>( a : T, b : T ) : Int {
		if (a == null && b == null) return 0;
		return
		if (a == null) 1 else if (b == null) -1 else
		( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		if( f1 == f2 )
			return true;
		if( !isFunction(f1) || !isFunction(f2) )
			return false;

		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {

		return switch (Type.typeof(v)) {
			case TObject, TClass(_): true;
			case _ : false;
		}
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return v != Enum && Builtin.isinstance(v, untyped Enum);
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		if( !hasField(o,field) ) return false;
		untyped __python__("o.__delattr__")(field);
		return true;
	}

	public static function copy<T>( o : T ) : T {
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return throw "not implemented";
	}

}
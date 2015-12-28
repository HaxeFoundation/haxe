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
import lua.Lua;
import lua.Boot;
@:coreApi class Reflect {
	static var hidden = ["__id__", "hx__closures", "super", "__index", "new", "prototype", "__fields"];

	public inline static function hasField( o : Dynamic, field : String ) : Bool {
		// TODO: Lua can't detect fields that are set to null, figure out a workaround.
		return untyped o.__fields != null ? o.__fields[field] != null :  o[field] != null;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return try o[field] catch( e : Dynamic ) null;
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		o[field] = value;
	}

	public static inline function getProperty( o : Dynamic, field : String ) : Dynamic {
		var tmp : Dynamic;
		return if( o == null ) {
				untyped __define_feature__("Reflect.getProperty",null);
			} else if( o.__properties__ != null && Reflect.field(o, "get_" + field) != null){
				callMethod(o, Reflect.field(o,"get_" + field), []);
			} else {
				Reflect.field(o,field);
			}
	}

	public static inline function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		var tmp : String;
		if( o.__properties__ && o.__properties__["set_"+field]) {
			tmp = o.__properties__["set_" + field];
			if (o.__name__ != null){
				callMethod(null,Reflect.field(o, tmp), [value]) ;
			} else {
				callMethod(o,Reflect.field(o, tmp), [value]) ;
			}
		} else {
			o[field] = __define_feature__("Reflect.setProperty",value);
		}
	}

	public inline static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic  {
		if (args == null || args.length == 0){
			return func(o);
		} else {
			var new_args:lua.Table<Int,String> = untyped __lua_table__(o);
			// Lua's table concat will skip the first element since it starts from 1
			if (o != null){
				new_args[2] = args[0];
			} else {
				new_args[1] = args[0];
			}
			new_args = lua.PairTools.ipairsConcat(new_args, cast args);
			return func(lua.Table.unpack(new_args));
		}
	}

	public static function fields( o : Dynamic ) : Array<String> {
		if (untyped o.__fields != null) return lua.PairTools.pairsFold(o.__fields, function(a,b,c:Array<String>){
			c.push(a);
			return c;
		}, []);
		else return lua.PairTools.pairsFold(o, function(a,b,c:Array<String>){
			if (hidden.indexOf(a) == -1) c.push(cast a);
			return c;
		}, []);
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return Lua.type(f) == "function" && !(Boot.isClass(f) || Boot.isEnum(f));
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return ( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return f1 == f2;
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		if( v == null )
			return false;
		var t = __lua__("type(v)");
		return (t == "string" || (t == "table" && v.__enum__ == null)) || (t == "function" && (lua.Boot.isClass(v) || lua.Boot.isEnum(v)) != null);
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return v != null && v.__enum__ != null;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool untyped {
		if( !hasField(o,field) ) return false;
		o[field] = null;
		o.__fields[field]=null;
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
		return function(a) {
			return f(untyped __lua__("unpack")(a));
		};
	}

}

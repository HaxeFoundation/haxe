/*
 * Copyright (C)2005-2017 Haxe Foundation
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
import lua.Table;

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T> untyped {
		if( o == null )
			return null;
		return lua.Boot.getClass(o);
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		if( o == null )
			return null;
		return o.__enum__;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		return c.__super__;
	}


	public static function getClassName( c : Class<Dynamic> ) : String {
		if (untyped c.__name__ == null) return null;
		return lua.Table.concat(untyped c.__name__,'.');
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		if (untyped e.__ename__ == null) return null;
		return lua.Table.concat(untyped e.__ename__,'.');
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
		// TODO: better tmp name for _hxClasses
		var cl : Class<Dynamic> = _hxClasses[name];
		// ensure that this is a class
		if( cl == null || !lua.Boot.isClass(cl) )
			return null;
		return cl;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		// TODO: better tmp name for _hxClasses
		var e : Dynamic = _hxClasses[name];
		// ensure that this is an enum
		if( e == null || !lua.Boot.isEnum(e) )
			return null;
		return e;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		return __new__(cl, lua.TableTools.unpack(cast args, 0));
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		var ret = __lua_table__();
		Lua.setmetatable(ret, untyped {__index : cl.prototype});
		return ret;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var f:Dynamic = Reflect.field(e,constr);
		if( f == null ) throw "No such constructor "+constr;
		if( Reflect.isFunction(f) ) {
			if( params == null ) throw "Constructor "+constr+" need parameters";
			return Reflect.callMethod(null,f,params);
		}
		if( params != null && params.length != 0 )
			throw "Constructor "+constr+" does not need parameters";
		return f;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var c : String = (untyped e.__constructs__)[index];
		if( c == null ) throw index+" is not a valid enum constructor index";
		return createEnum(e,c,params);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		var p : Dynamic = untyped c.prototype;
		var a :Array<String> = [];
		while (p != null){
			for (f in lua.Boot.fieldIterator(p)){
				if (!Lambda.has(a,f)) a.push(f);
			}
			var mt = lua.Lua.getmetatable(p);
			if (mt != null && mt.__index != null ) p = mt.__index;
			else p = null;
		}
		return a;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		var a = Reflect.fields(c);
		a.remove("__name__");
		a.remove("__interfaces__");
		a.remove("__properties__");
		a.remove("__super__");
		a.remove("__meta__");
		a.remove("prototype");
		a.remove("new");
		return a;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var a : Array<String> = untyped e.__constructs__;
		return a.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType  {

		switch( Lua.type(v) ) {
			case "boolean": return TBool;
			case "string": return TClass(String);
			case "number":
						   // this should handle all cases : NaN, +/-Inf and Floats outside range
						   if( Math.ceil(v) == v%2147483648.0 )
							   return TInt;
						   return TFloat;
			case "table":
						   var e = v.__enum__;
						   if( e != null )
							   return TEnum(e);
						   var c = lua.Boot.getClass(v);
						   if( c != null )
							   return TClass(c);
						   return TObject;
			case "function":
						   if( lua.Boot.isClass(v) || lua.Boot.isEnum(v) )
							   return TObject;
						   return TFunction;
			case "nil":
						   return TNull;
			default:
						   return TUnknown;
		}
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
		try {
			if( a[0] != b[0] )
				return false;
			for( i in 2...a.length )
				if( !enumEq(a[i],b[i]) )
					return false;
			var e = a.__enum__;
			if( e != b.__enum__ || e == null )
				return false;
		} catch( e : Dynamic ) {
			return false;
		}
		return true;
	}

	public inline static function enumConstructor( e : EnumValue ) : String {
		return untyped e[0];
	}

	public inline static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return (cast e : Array<Dynamic>).slice(2);
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e[1];
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		return untyped e.__empty_constructs__;
	}

}


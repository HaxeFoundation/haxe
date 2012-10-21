/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

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
		return js.Boot.getClass(o);
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
		var a : Array<String> = untyped c.__name__;
		return a.join(".");
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		var a : Array<String> = untyped e.__ename__;
		return a.join(".");
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
		var cl : Class<Dynamic> = $hxClasses[name];
		// ensure that this is a class
		if( cl == null || !js.Boot.isClass(cl) )
			return null;
		return cl;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		var e : Dynamic = $hxClasses[name];
		// ensure that this is an enum
		if( e == null || !js.Boot.isEnum(e) )
			return null;
		return e;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		switch( args.length ) {
		case 0:
			return __new__(cl);
		case 1:
			return __new__(cl,args[0]);
		case 2:
			return __new__(cl,args[0],args[1]);
		case 3:
			return __new__(cl,args[0],args[1],args[2]);
		case 4:
			return __new__(cl,args[0],args[1],args[2],args[3]);
		case 5:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4]);
		case 6:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5]);
		case 7:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
		case 8:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
		default:
			throw "Too many arguments";
		}
		return null;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		__js__("function empty() {}; empty.prototype = cl.prototype");
		return __js__("new empty()");
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var f = Reflect.field(e,constr);
		if( f == null ) throw "No such constructor "+constr;
		if( Reflect.isFunction(f) ) {
			if( params == null ) throw "Constructor "+constr+" need parameters";
			return Reflect.callMethod(e,f,params);
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
		var a = [];
		untyped __js__("for(var i in c.prototype) a.push(i)");
		a.remove("__class__");
		a.remove("__properties__");
		return a;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		var a = Reflect.fields(c);
		a.remove("__name__");
		a.remove("__interfaces__");
		a.remove("__properties__");
		a.remove("__super__");
		a.remove("prototype");
		return a;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var a : Array<String> = untyped e.__constructs__;
		return a.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
		switch( __js__("typeof")(v) ) {
		case "boolean": return TBool;
		case "string": return TClass(String);
		case "number":
			// this should handle all cases : NaN, +/-Inf and Floats outside range
			if( Math.ceil(v) == v%2147483648.0 )
				return TInt;
			return TFloat;
		case "object":
			if( v == null )
				return TNull;
			var e = v.__enum__;
			if( e != null )
				return TEnum(e);
			var c = js.Boot.getClass(v);
			if( c != null )
				return TClass(c);
			return TObject;
		case "function":
			if( js.Boot.isClass(v) || js.Boot.isEnum(v) )
				return TObject;
			return TFunction;
		case "undefined":
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
		return untyped e.slice(2);
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e[1];
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		var all = [];
		var cst : Array<String> = untyped e.__constructs__;
		for( c in cst ) {
			var v = Reflect.field(e,c);
			if( !Reflect.isFunction(v) )
				all.push(v);
		}
		return all;
	}

}


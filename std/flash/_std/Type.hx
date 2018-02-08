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
		var cname = __global__["flash.utils.getQualifiedClassName"](o);
		if( cname == "null" || cname == "Object" || cname == "int" || cname == "Number" || cname == "Boolean" )
			return null;
		if( o.hasOwnProperty("prototype") )
			return null;
		var c = __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
		if( c.__isenum )
			return null;
		return c;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		var cname = __global__["flash.utils.getQualifiedClassName"](o);
		if( cname == "null" || cname.substr(0,8) == "builtin." )
			return null;
		// getEnum(Enum) should be null
		if( o.hasOwnProperty("prototype") )
			return null;
		var c = __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
		if( !c.__isenum )
			return null;
		return c;
	}


	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		var cname = __global__["flash.utils.getQualifiedSuperclassName"](c);
		if( cname == null || cname == "Object" )
			return null;
		return __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		if( c == null )
			return null;
		var str : String = untyped __global__["flash.utils.getQualifiedClassName"](c);
		switch( str ) {
		case "int": return "Int";
		case "Number": return "Float";
		case "Boolean": return "Bool";
		#if as3
		case "Object": return "Dynamic";
		#end
		default:
		}
		var parts = str.split("::");
		#if as3
		if(parts[parts.length - 1] == "_Object") {
			parts[parts.length - 1] = "Object";
		}
		#end
		return parts.join(".");
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return getClassName(cast e);
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
		var cl : Class<Dynamic>;
		try {
			cl = __as__(__global__["flash.utils.getDefinitionByName"](name),Class);
			if( cl.__isenum )
				return null;
			return cl; // skip test below
		} catch( e : Dynamic ) {
			switch( name ) {
			case "Int": return Int;
			case "Float": return Float;
			#if as3
			case "Dynamic": return Dynamic;
			#end
			}
			return null;
		}
		// ensure that this is a class
		if( cl == null || cl.__name__ == null )
			return null;
		return cl;
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		var e : Dynamic;
		try {
			e = __global__["flash.utils.getDefinitionByName"](name);
			if( !e.__isenum )
				return null;
			return e;
		} catch( e : Dynamic ) {
			if( name == "Bool" ) return Bool;
			return null;
		}
		// ensure that this is an enum
		if( e == null || e.__ename__ == null )
			return null;
		return e;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		return switch( args.length ) {
		case 0: __new__(cl);
		case 1: __new__(cl,args[0]);
		case 2: __new__(cl,args[0],args[1]);
		case 3: __new__(cl,args[0],args[1],args[2]);
		case 4: __new__(cl,args[0],args[1],args[2],args[3]);
		case 5: __new__(cl,args[0],args[1],args[2],args[3],args[4]);
		case 6: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5]);
		case 7: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
		case 8: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
		case 9: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8]);
		case 10: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9]);
		case 11: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9],args[10]);
		case 12: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9],args[10],args[11]);
		case 13: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9],args[10],args[11],args[12]);
		case 14: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9],args[10],args[11],args[12],args[13]);
		default: throw "Too many arguments";
		}
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		try {
			flash.Boot.skip_constructor = true;
			var i = __new__(cl);
			flash.Boot.skip_constructor = false;
			return i;
		} catch( e : Dynamic ) {
			flash.Boot.skip_constructor = false;
			throw e;
		}
		return null;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var f:Dynamic = untyped e[constr];
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

	static function describe( t : Dynamic, fact : Bool ) : Array<String> untyped {
		var fields = new Array();
		var xml : flash.xml.XML = __global__["flash.utils.describeType"](t);
		if( fact )
			xml = xml.factory[0];
		var methods = xml.child("method");
		for( i in 0...methods.length() )
			fields.push( Std.string(methods[i].attribute("name")) );
		var vars = xml.child("variable");
		for( i in 0...vars.length() )
			fields.push( Std.string(vars[i].attribute("name")) );
		var accs = xml.child("accessor");
		for( i in 0...accs.length() )
			fields.push( Std.string(accs[i].attribute("name")) );
		return fields;
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return describe(c,true);
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		var a = describe(c,false);
		a.remove("__construct__");
		a.remove("prototype");
		return a;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var a : Array<String> = untyped e.__constructs__;
		return a.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
		var cname = __global__["flash.utils.getQualifiedClassName"](v);
		switch(cname) {
		case "null": return TNull;
		case "void": return TNull; // undefined
		case "int": return TInt;
		case "Number":
			// integers >28 bits are stored as Numbers in avm2
			if( (v < -0x10000000 || v >= 0x10000000) && Std.int(v) == v )
				return TInt;
			return TFloat;
		case "Boolean": return TBool;
		case "Object": return TObject;
		case "Function": return TFunction;
		default:
			var c : Dynamic = null;
			try {
				c = __global__["flash.utils.getDefinitionByName"](cname);
				if( v.hasOwnProperty("prototype") )
					return TObject;
				if( c.__isenum )
					return TEnum(c);
				return TClass(c);
			} catch( e : Dynamic ) {
				if( cname == "builtin.as$0::MethodClosure" || cname.indexOf("-") != -1 )
					return TFunction;
				return if( c == null ) TFunction else TClass(c);
			}
		}
		return null;
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
		try {
			if( a.index != b.index )
				return false;
			var ap : Array<Dynamic> = a.params;
			var bp : Array<Dynamic> = b.params;
			for( i in 0...ap.length )
				if( !enumEq(ap[i],bp[i]) )
					return false;
		} catch( e : Dynamic ) {
			return false;
		}
		return true;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return untyped e.tag;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return untyped if( e.params == null ) [] else e.params;
	}

	@:extern
	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
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


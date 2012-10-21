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
		if( __dollar__typeof(o) != __dollar__tobject )
			return null;
		var p = __dollar__objgetproto(o);
		if( p == null )
			return null;
		return p.__class__;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		if( __dollar__typeof(o) != __dollar__tobject )
			return null;
		return o.__enum__;
	}


	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		return c.__super__;
	}


	public static function getClassName( c : Class<Dynamic> ) : String {
		if( c == null )
			return null;
		var a : Array<String> = untyped c.__name__;
		return a.join(".");
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		var a : Array<String> = untyped e.__ename__;
		return a.join(".");
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
		var path = name.split(".");
		cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
		var i = 1;
		while( cl != null && i < path.length ) {
			cl = Reflect.field(cl,path[i]);
			i += 1;
		}
		// ensure that this is a class
		if( cl == null || cl.__name__ == null )
			return null;
		return cl;
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		var path = name.split(".");
		e = Reflect.field(neko.Boot.__classes,path[0]);
		var i = 1;
		while( e != null && i < path.length ) {
			e = Reflect.field(e,path[i]);
			i += 1;
		}
		// ensure that this is an enum
		if( e == null || e.__ename__ == null )
			return null;
		return e;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		return __dollar__call(__dollar__objget(cl,__dollar__hash("new".__s)),cl,args.__neko());
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		var o = __dollar__new(null);
		__dollar__objsetproto(o,cl.prototype);
		return o;
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
		var a = Reflect.fields(untyped c.prototype);
		c = untyped c.__super__;
		while( c != null ) {
			for( f in Reflect.fields(untyped c.prototype) ) {
				a.remove(f);
				a.push(f);
			}
			c = untyped c.__super__;
		}
		a.remove("__class__");
		a.remove("__serialize");
		a.remove("__string");
		a.remove("__properties__");
		return a;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		var a = Reflect.fields(c);
		a.remove("__name__");
		a.remove("__interfaces__");
		a.remove("__super__");
		a.remove("__string");
		a.remove("__construct__");
		a.remove("__properties__");
		a.remove("prototype");
		a.remove("new");
		#if (macro || interp)
		a.remove("__ct__");
		#end
		return a;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var a : Array<String> = untyped e.__constructs__;
		return a.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
		return switch( __dollar__typeof(v) ) {
		case __dollar__tnull: TNull;
		case __dollar__tint: TInt;
		case __dollar__tfloat: TFloat;
		case __dollar__tbool: TBool;
		case __dollar__tfunction: TFunction;
		case __dollar__tobject:
			var c = v.__class__;
			if( c != null )
				TClass(c);
			else {
				var e = v.__enum__;
				if( e != null )
					TEnum(e);
				else
					TObject;
			}
		default: TUnknown;
		}
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
		try {
			if( a.__enum__ == null || a.index != b.index )
				return false;
		} catch( e : Dynamic ) {
			return false;
		}
		for( i in 0...__dollar__asize(a.args) )
			if( !enumEq(a.args[i],b.args[i]) )
				return false;
		return true;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return new String(untyped e.tag);
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return untyped if( e.args == null ) [] else Array.new1(e.args,__dollar__asize(e.args));
	}

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


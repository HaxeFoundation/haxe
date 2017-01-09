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
		var cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
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
		var e = Reflect.field(neko.Boot.__classes,path[0]);
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
		var fnew = $objget(cl,$hash("new".__s));
		var a = args.__neko();
		// pad missing args with null's
		var n = $nargs(fnew);
		if( n > $asize(a) ) {
			var a2 = $amake(n);
			$ablit(a2,0,a,0,$asize(a));
			a = a2;
		}
		return $call(fnew,cl,a);
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		var o = __dollar__new(null);
		__dollar__objsetproto(o,cl.prototype);
		return o;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var f:Dynamic = Reflect.field(e,constr);
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
		case 0: TNull;
		case 1: TInt;
		case 2: TFloat;
		case 3: TBool;
		case 7: TFunction;
		case 5:
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


import python.lib.Builtin;

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

@:access(python.Boot)
@:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T> {

		

		if( o == null )
			return null;
		

		if (python.Boot.isClass(o)) return null;

		if (python.Boot.isAnonObject(o)) return null;

		if (Builtin.hasattr(o, "_hx_class")) {	
			return untyped o._hx_class;
		}
		if (Builtin.hasattr(o, "__class__")) {	
			return untyped o.__class__;
		} else {
			return null;
		}
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		if( o == null )
			return null;
		return untyped o.__class__;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		if( c == null )
			return null;
		
		try {
			if (Builtin.hasattr(c, "_hx_super")) {
				return untyped c._hx_super;
			}
			return untyped __python_array_get(c.__bases__,0);
		} catch (e:Dynamic) {

		}
		return null;

	}


	public static function getClassName( c : Class<Dynamic> ) : String 
	{
		
		if (Builtin.hasattr(c, "_hx_class_name")) {
			return untyped c._hx_class_name;
		} else {
			// not a haxe class
			if (c == Array) return "Array";
			if (c == Math) return "Math";
			if (c == String) return "String";

			try {
				var s :String = untyped c.__name__;
			} catch (e:Dynamic) {
			
			}
		}
		var res = null;
		

		return res;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return untyped e._hx_class_name;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		
		if (name == "Array") return Array;
		if (name == "Math") return Math;
		if (name == "String") return String;
		var cl : Class<Dynamic> = (untyped _hx_classes : python.lib.Types.Dict<String, Class<Dynamic>>).get(name, null);
        // ensure that this is a class
        if( cl == null || !python.Boot.isClass(cl) )
                return null;
        return cl;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		if (name == "Bool") return cast Bool;
		var o = resolveClass(name);
		return if (Builtin.hasattr(o, "_hx_constructs")) cast o else null;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped 
	{
		var l = args.length;
		switch( l ) 
		{
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

	public static function createEmptyInstance<T>( cl : Class<T> ) : T 
	{
		var i = untyped cl.__new__(cl);

		function callInit (cl) {
			var sc = getSuperClass(cl);
			if (sc != null) {
				callInit(sc);
			}
			if (Builtin.hasattr(cl, "_hx_empty_init")) {
				
				untyped cl._hx_empty_init(i);
			}	
		}
		callInit(cl);
		
		return i;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T 
	{
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

		var c : String = (untyped e._hx_constructs)[index];
		if( c == null ) throw index+" is not a valid enum constructor index";
		return createEnum(e,c,params);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		// dict((name, getattr(f, name)) for name in dir(c) if not name.startswith('__'))
		var f = if (Builtin.hasattr(c, "_hx_fields")) {
			var x:Array<String> = untyped c._hx_fields;
			var x2:Array<String> = untyped c._hx_methods;
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

		
		//return throw "getInstanceFields not implemented";
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		if (Builtin.hasattr(c, "_hx_statics")) {
			var x:Array<String> = untyped c._hx_statics;
			return x.copy();
		} else {
			return [];
		}
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		if (Builtin.hasattr(e, "_hx_constructs")) {
			var x:Array<String> = untyped e._hx_constructs;
			return x.copy();
		} else {
			return [];
		}
	}



	public static function typeof( v : Dynamic ) : ValueType {
		if (v == null) {
			return TNull;
		} else if (Builtin.isinstance(v, untyped __python__("bool") )) {
			return TBool;
		} else if (Builtin.isinstance(v, untyped __python__("int"))) {
			return TInt;
		} else if (Builtin.isinstance(v, untyped __python__("float"))) {
			return TFloat;
		} else if (Builtin.isinstance(v, String)) {
			return TClass(String);
		} else if (Builtin.isinstance(v, Array)) {
			return TClass(Array);
		} else if (Builtin.isinstance(v, untyped __python__("_hx_c._hx_AnonObject")) || python.lib.Inspect.isclass(v)) {
			return TObject;
		}
		else if (Builtin.isinstance(v, untyped __python__("_hx_c.Enum"))) {
			return TEnum(untyped v.__class__);
		}
		else if (Builtin.isinstance(v, untyped __python__("type")) || Builtin.hasattr(v, "_hx_class")) {
			return TClass(untyped v.__class__);
		} else if (Builtin.callable(v)) {
			return TFunction;
		} else {
			return TUnknown;
		}
	}

	public static function enumEq<T>( a : T, b : T ) : Bool {
		if( a == b )
			return true;
		try {
			if (b == null && a != b) return false;
			if( untyped a.tag != untyped b.tag )
				return false;
			var p1:Array<Dynamic> = untyped a.params;
			var p2:Array<Dynamic> = untyped b.params;
			if (p1.length != p2.length) return false;

			for( i in 0...p1.length )
				if( !enumEq(p1[i],p2[i]) )
					return false;
			//var e = Type.getClass(a);

			if( untyped a._hx_class != untyped b._hx_class)
				return false;
		} catch( e : Dynamic ) {
			return false;
		}
		return true;
	}

	public static inline function enumConstructor( e : EnumValue ) : String {
		return untyped e.tag;
	}

	public static inline function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return untyped e.params;
	}

	public static inline function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T>
    {
            var ctors = getEnumConstructs(e);
            var ret = [];
            for (ctor in ctors)
            {
                    var v = Reflect.field(e, ctor);
                    if (Std.is(v, e))
                            ret.push(v);
            }

            return ret;
    }

}

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
@:coreApi class Reflect {

	public inline static function hasField( o : Dynamic, field : String ) : Bool untyped {
		return __this__["hasOwnProperty"]["call"](o,field);
	}

	public inline static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return o[field];
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		o[field] = value;
	}

	static function findAccessor( c : Class<Dynamic>, name : String ):Dynamic untyped {
		do {
			var getter = c.__properties__[name];
			if (getter != null)
				return getter;
			c = c.__super__;
		} while (c != null);
		return null;
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic untyped {
		var getter = findAccessor( Std.is(o,Class) ? o : o.__class__, "get_" +field);
		return if (getter != null)
			o[getter]["apply"](o, [field]);
		else
			Reflect.field(o, field);
	}

	public static inline function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		var setter = findAccessor( Std.is(o,Class) ? o : o.__class__, "set_" +field);
		return if (setter != null)
			o[setter]["apply"](o, [value]);
		else
			Reflect.setField(o, field, value);
	}

	public inline static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		return func["apply"](o,args);
	}

	public static function fields( o : Dynamic ) : Array<String> untyped {
		if( o == null ) return new Array();
		var a : Array<String> = __keys__(o);
		var i = 0;
		while( i < a.length ) {
			if( a[i] == "__id__" || !a["hasOwnProperty"]["call"](o,a[i]) )
				a.splice(i,1);
			else
				++i;
		}
		return a;
	}

	public static function isFunction( f : Dynamic ) : Bool untyped {
		return __typeof__(f) == "function" && f.__name__ == null;
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return ( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return untyped f1 == f2 || (f1["f"] == f2["f"] && f1["o"] == f2["o"] && f1["f"] != null);
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		var t = __typeof__(v);
		return (t == "string" || (t == "object" && !v.__enum__) || (t == "function" && v.__name__ != null));
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool untyped {
		if( __this__["hasOwnProperty"]["call"](o,field) != true ) return false;
		__delete__(o,field);
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
		return function() { return f(untyped __arguments__); };
	}

}

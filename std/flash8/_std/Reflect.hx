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
			if( !a["hasOwnProperty"]["call"](o,a[i]) )
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

	public static function deleteField( o : Dynamic, f : String ) : Bool untyped {
		if( __this__["hasOwnProperty"]["call"](o,f) != true ) return false;
		__delete__(o,f);
		return true;
	}

	public static function copy<T>( o : T ) : T {
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}

	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return function() { return f(untyped __arguments__); };
	}

}

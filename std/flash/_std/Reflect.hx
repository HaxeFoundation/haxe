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

	public static function hasField( o : Dynamic, field : String ) : Bool untyped {
		return o.hasOwnProperty( field );
	}

	public static function field( o : Dynamic, field : String ) : Dynamic untyped {
		// sealed classes will throw an exception
		return try o[field] catch( e : Dynamic ) null;
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		o[field] = value;
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic untyped {
		try {
			return o["get_" + field]();
		} catch( e : Dynamic ) {
			return o[field];
		} catch( e : Dynamic ) {
			return null;
		}
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		try {
			o["set_" + field](value);
		} catch( e : Dynamic ) {
			o[field] = value;
		}
	}

	public inline static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		return func.apply(o,args);
	}

	public static function fields( o : Dynamic ) : Array<String> untyped {
		if( o == null ) return new Array();
		#if as3
		var a : Array<String> = __keys__(o);
		var i = 0;
		while( i < a.length ){
			if( !o.hasOwnProperty(a[i]) )
				a.splice(i,1);
			else
				++i;
		}
		#else
		var i = 0;
		var a = [];
		while( untyped __has_next__(o,i) ) {
			var prop = untyped __forin__(o,i);
			if( o.hasOwnProperty(prop) )
				a.push(prop);
		}
		#end
		return a;
	}

	public static function isFunction( f : Dynamic ) : Bool untyped {
		return __typeof__(f) == "function";
	}

	public static function compare<T>( a : T, b : T ) : Int {
		var a : Dynamic = a;
		var b : Dynamic = b;
		return ( a == b ) ? 0 : ((a > b) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return f1 == f2; // VM-level closures
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		if( v == null )
			return false;
		var t = __typeof__(v);
		if( t == "object" ) {
			try {
				if( v.__enum__ == true )
					return false;
			} catch( e : Dynamic ) {
			}
			return true;
		}
		return (t == "string");
	}

	public static function deleteField( o : Dynamic, f : String ) : Bool untyped {
		if( o.hasOwnProperty(f) != true ) return false;
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
		return function(__arguments__) { return f(__arguments__); };
	}

}

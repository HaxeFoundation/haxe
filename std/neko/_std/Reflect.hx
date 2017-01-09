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
@:coreApi class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool untyped {
		return $typeof(o) == $tobject && $objfield(o,$fasthash(field.__s));
	}

	public inline static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return if( $typeof(o) != $tobject ) null else $objget(o,$fasthash(field.__s));
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		if( $typeof(o) == $tobject )
			$objset(o,$hash(field.__s),value);
	}

	public static inline function getProperty( o : Dynamic, field : String ) : Dynamic untyped {
		var tmp;
		return if( $typeof(o) != $tobject ) null else if( o.__properties__ != null && (tmp=$objget(o.__properties__,$fasthash("get_".__s+field.__s))) != null ) $call($objget(o,$fasthash(tmp)),o,$array()) else $objget(o,$fasthash(field.__s));
	}

	public static inline function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		if( $typeof(o) == $tobject ) {
			var tmp;
			if( o.__properties__ != null && (tmp=$objget(o.__properties__,$fasthash("set_".__s+field.__s))) != null ) $call($objget(o,$fasthash(tmp)),o,$array(value)) else $objset(o,$hash(field.__s),value);
		}
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic untyped {
		var a = args.__neko();
		// pad missing args with null's
		var n = $nargs(func);
		if( n > $asize(a) ) {
			var a2 = $amake(n);
			$ablit(a2,0,a,0,$asize(a));
			a = a2;
		}
		return $call(func,o,a);
	}

	public static function fields( o : Dynamic ) : Array<String> untyped {
		if( $typeof(o) != $tobject )
			return new Array<String>();
		else {
			var a : neko.NativeArray<Int> = $objfields(o);
			var i = 0;
			var hasid = false;
			var l = $asize(a);
			while( i < l ) {
				var fid = a[i];
				if( fid == -190054693 ) hasid = true; // $hash("__id__")
				a[i] = new String($field(fid));
				i++;
			}
			var a : Array<String> = Array.new1(a, l);
			if( hasid ) a.remove("__id__");
			return a;
		}
	}

	public static function isFunction( f : Dynamic ) : Bool untyped {
		return $typeof(f) == $tfunction;
	}

	public inline static function compare<T>( a : T, b : T ) : Int {
		return untyped $compare(a,b);
	}

	public inline static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return same_closure(f1,f2);
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		return $typeof(v) == $tobject && v.__enum__ == null;
	}

	public static function isEnumValue( v : Dynamic ) : Bool untyped {
		return $typeof(v) == $tobject && v.__enum__ != null;
	}

	public inline static function deleteField( o : Dynamic, field : String ) : Bool untyped {
		return $objremove(o,$fasthash(field.__s));
	}

	public inline static function copy<T>( o : T ) : T {
		return untyped $new(o);
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return untyped $varargs(function(a) { return f(Array.new1(a,$asize(a))); });
	}

	static var same_closure = try neko.Lib.load("std","same_closure",2) catch( e : Dynamic ) function(f1,f2) return f1 == f2;

}

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

@:core_api class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool untyped {
		return __dollar__typeof(o) == __dollar__tobject && __dollar__objfield(o,__dollar__hash(field.__s));
	}

	public inline static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return if( __dollar__typeof(o) != __dollar__tobject ) null else __dollar__objget(o,__dollar__hash(field.__s));
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		if( __dollar__typeof(o) == __dollar__tobject )
			__dollar__objset(o,__dollar__hash(field.__s),value);
	}

	public inline static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		return __dollar__call(func,o,args.__neko());
	}

	public static function fields( o : Dynamic ) : Array<String> untyped {
		if( __dollar__typeof(o) != __dollar__tobject )
			return new Array<String>();
		else {
			var a : neko.NativeArray<Int> = __dollar__objfields(o);
			var i = 0;
			var l = __dollar__asize(a);
			while( i < l ) {
				a[i] = new String(__dollar__field(a[i]));
				i++;
			}
			return Array.new1(a,l);
		}
	}

	public static function isFunction( f : Dynamic ) : Bool untyped {
		return __dollar__typeof(f) == __dollar__tfunction;
	}

	public inline static function compare<T>( a : T, b : T ) : Int {
		return untyped __dollar__compare(a,b);
	}

	public inline static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return same_closure(f1,f2);
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		return __dollar__typeof(v) == __dollar__tobject && v.__enum__ == null;
	}

	public inline static function deleteField( o : Dynamic, f : String ) : Bool untyped {
		return __dollar__objremove(o,__dollar__hash(f.__s));
	}

	public inline static function copy<T>( o : T ) : T {
		return untyped __dollar__new(o);
	}

	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return untyped __dollar__varargs(function(a) { return f(Array.new1(a,__dollar__asize(a))); });
	}

	#if neko
	static var same_closure = try neko.Lib.load("std","same_closure",2) catch( e : Dynamic ) function(f1,f2) return f1 == f2;
	#end

}

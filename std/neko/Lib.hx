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
package neko;

class Lib {

	/**
		Load and return a Neko primitive from a NDLL library.
	**/
	public static function load( lib : String, prim : String, nargs : Int ) : Dynamic {
		return untyped __dollar__loader.loadprim((lib+"@"+prim).__s,nargs);
	}

	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __dollar__print(v);
	}

	/**
		Print the specified value on the default output followed by a newline character.
	**/
	public static function println( v : Dynamic ) : Void {
		untyped __dollar__print(v,"\n");
	}

	/**
		Rethrow an exception. This is useful when manually filtering an exception in order
		to keep the previous exception stack.
	**/
	public static function rethrow( e : Dynamic ) : Dynamic {
		return untyped __dollar__rethrow(e);
	}

	/**
		Serialize using native Neko serialization. This will return a Binary string that can be
		stored for long term usage. The serialized data is optimized for speed and not for size.
	**/
	public static function serialize( v : Dynamic ) : String {
		return new String(__serialize(v));
	}

	/**
		Unserialize a string using native Neko serialization. See [serialize].
	**/
	public static function unserialize( s : String ) : Dynamic {
		return untyped __unserialize(s.__s,__dollar__loader);
	}
	
	/**
		Unserialize a string using native Neko serialization. See [serialize].
		This function assume that all the serialized data was serialized with current
		module, even if the module name was different. This can happen if you are unserializing
		some data into mod_neko that was serialized on a different server using a different
		file path.
	**/
	public static function localUnserialize( s : String ) : Dynamic {		
		return untyped __unserialize(s.__s,{
			loadmodule : function(m,l) { return __dollar__exports; },
			loadprim : function(p,n) { return __dollar__loader.loadprim(p,n); }			
		});
	}

	/**
		Creates a raw string of [size] bytes.
	**/
	public static function makeString( size : Int ) : String {
		return new String(untyped __dollar__smake(size));
	}

	/**
		Copy bytes between two strings.
	**/
	public static function copyBytes( dst : String, dst_pos : Int, src : String, src_pos : Int, len : Int ) : Void {
		untyped __dollar__sblit(dst.__s,dst_pos,src.__s,src_pos,len);
	}

	/**
		Converts a Neko value to its haXe equivalent. Used for wrapping String and Arrays raw values into haXe Objects.
	**/
	public static function nekoToHaxe( v : Dynamic ) : Dynamic untyped {
		switch( __dollar__typeof(v) ) {
		case __dollar__tnull: return v;
		case __dollar__tint: return v;
		case __dollar__tfloat: return v;
		case __dollar__tbool: return v;
		case __dollar__tstring: return new String(v);
		case __dollar__tarray:
			var a = Array.new1(v,__dollar__asize(v));
			for( i in 0...a.length )
				a[i] = nekoToHaxe(a[i]);
			return a;
		case __dollar__tobject:
			var f = __dollar__objfields(v);
			var i = 0;
			var l = __dollar__asize(f);
			var o = __dollar__new(v);
			if( __dollar__objgetproto(f) != null )
				throw "Can't convert object prototype";
			while( i < l ) {
				__dollar__objset(o,f[i],nekoToHaxe(__dollar__objget(v,f[i])));
				i += 1;
			}
			return o;
		default:
			throw "Can't convert "+string(v);
		}
	}

	/**
		Converts a Neko value to its haXe equivalent. Used to unwrap String and Arrays Objects into raw Neko values.
	**/
	public static function haxeToNeko( v : Dynamic ) : Dynamic untyped {
		switch( __dollar__typeof(v) ) {
		case __dollar__tnull: return v;
		case __dollar__tint: return v;
		case __dollar__tfloat: return v;
		case __dollar__tbool: return v;
		case __dollar__tobject:
			var cl = v.__class__;
			if( cl == String )
				return v.__s;
			if( cl == Array ) {
				var a = untyped __dollar__amake(v.length);
				for( i in 0...a.length )
					a[i] = haxeToNeko(v[i]);
				return a;
			}
			if( cl != null || __dollar__objgetproto(v) != null )
				throw "Can't convert "+string(v);
			var f = __dollar__objfields(v);
			var i = 0;
			var l = __dollar__asize(f);
			var o = __dollar__new(v);
			while( i < l ) {
				__dollar__objset(o,f[i],haxeToNeko(__dollar__objget(v,f[i])));
				i += 1;
			}
			return o;
		default:
			throw "Can't convert "+string(v);
		}
	}
	
	public static function getClasses() : Dynamic {
		return untyped neko.Boot.__classes;
	}

	static var __serialize = load("std","serialize",1);
	static var __unserialize = load("std","unserialize",2);

}

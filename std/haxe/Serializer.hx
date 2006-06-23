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
package haxe;

class Serializer {

	var buf : StringBuf;
	var cache : Array<Dynamic>;

	public function new() {
		buf = new StringBuf();
		cache = new Array();
	}

	public function toString() {
		return buf.toString();
	}

	/* prefixes :
		n : null
		t : true
		f : false
		i : Int
		z : zero
		d : Float
		e : reserved (float exp)
		k : NaN
		m : -Inf
		p : +Inf
		s : string
		j : utf8 string
		a : array
		u : array nulls
		h : array end
		o : object
		g : object end
		r : reference
		c : class
		w : enum
	*/

	public function bytes(s : String) {
		#if neko
		return s.length;
		#else true
		var b = s.length;
		for( i in 0...s.length ) {
			var c = s.charCodeAt(i);
			if( c < 0x7F )
				continue;
			if( c < 0x7FF ) {
				b++;
				continue;
			}
			if( c < 0xFFFF ) {
				b += 2;
				continue;
			}
			b += 3;
		}
		return b;
		#end
	}

	function serializeString( s : String ) {
		if( serializeRef(s) )
			return;
		for( i in 0...s.length ) {
			var c = s.charCodeAt(i);
			if( c > 0x7F || c == 13 || c == 10 ) {
				s = s.split("\\").join("\\\\").split("\n").join("\\n").split("\r").join("\\r");
				buf.add("j");
				buf.add(bytes(s));
				buf.add(":");
				buf.add(s);
				return;
			}
		}
		buf.add("s");
		buf.add(s.length);
		buf.add(":");
		buf.add(s);
	}

	function serializeRef(v) {
		#if js
		var vt = untyped __js__("typeof")(v);
		#end
		for( i in 0...cache.length ) {
			#if js
			var ci = cache[i];
			if( untyped __js__("typeof")(ci) == vt && ci == v ) {
			#else true
			if( cache[i] == v ) {
			#end
				buf.add("r");
				buf.add(i);
				return true;
			}
		}
		cache.push(v);
		return false;
	}

	function serializeEnum(v : Dynamic) {
		buf.add("w");
		serialize(v.__enum__.__ename__);
		#if neko
		serializeString(new String(v.tag));
		if( v.args == null )
			buf.add(0);
		else {
			var l : Int = untyped __dollar__asize(v.args);
			buf.add(l);
			for( i in 0...l )
				serialize(v.args[i]);
		}
		#else true
		serializeString(v[0]);
		var l = v[untyped "length"];
		buf.add(l - 1);
		for( i in 1...l )
			serialize(v[i]);
		#end
	}

	public function serialize( v : Dynamic ) {
		if( v == null ) {
			buf.add("n");
			return;
		}
		if( Std.is(v,Int) ) {
			if( v == 0 ) {
				buf.add("z");
				return;
			}
			buf.add("i");
			buf.add(Std.string(v));
			return;
		}
		if( Std.is(v,Float) ) {
			if( Math.isNaN(v) )
				buf.add("k");
			else if( !Math.isFinite(v) )
				buf.add(if( v < 0 ) "m" else "p");
			else {
				buf.add("d");
				buf.add(Std.string(v));
			}
			return;
		}
		#if neko
		var t = untyped __dollar__typeof(v);
		if( t == untyped __dollar__tstring )
			v = new String(v);
		#end
		if( Std.is(v,String) ) {
			serializeString(v);
			return;
		}
		#if neko
		if( t == untyped __dollar__tarray )
			t = untyped Array.new1(v,__dollar__asize(v));
		#end
		if( Std.is(v,Array) ) {
			if( serializeRef(v) )
				return;
			#if neko
			#else true
			var e : Dynamic = v.__enum__;
			if( e != null && e.__ename__ != null ) {
				serializeEnum(v);
				return;
			}
			#end
			var ucount = 0;
			buf.add("a");
			var l = #if neko v.length #else true v[untyped "length"] #end;
			for( i in 0...l ) {
				if( v[i] == null )
					ucount++;
				else {
					if( ucount > 0 ) {
						if( ucount == 1 )
							buf.add("n");
						else {
							buf.add("u");
							buf.add(ucount);
						}
						ucount = 0;
					}
					serialize(v[i]);
				}
			}
			if( ucount > 0 ) {
				if( ucount == 1 )
					buf.add("n");
				else {
					buf.add("u");
					buf.add(ucount);
				}
			}
			buf.add("h");
			return;
		}
		if( v == true ) {
			buf.add("t");
			return;
		}
		if( v == false ) {
			buf.add("f");
			return;
		}
		if( serializeRef(v) )
			return;
		if( Reflect.isFunction(v) )
			throw "Cannot serialize function";
		#if neko
		if( t == untyped __dollar__tabstract )
			throw "Cannot serialize abstract";
		#end
		var c : Dynamic = v.__class__;
		if( c != null && c.__name__ != null ) {
			buf.add("c");
			serialize(c.__name__);
		} else {
			#if neko
			var e : Dynamic = v.__enum__;
			if( e != null && e.__ename__ != null ) {
				serializeEnum(v);
				return;
			}
			#end
			buf.add("o");
		}
		var fl = Reflect.fields(v);
		for( f in fl ) {
			serializeString(f);
			serialize(Reflect.field(v,f));
		}
		buf.add("g");
	}

	public function serializeException( e : Dynamic ) {
		buf.add("x");
		serialize(e);
	}

	/**
		Serialize a single value and return the string.
	**/
	public static function run( v : Dynamic ) {
		var s = new Serializer();
		s.serialize(v);
		return s.toString();
	}

}


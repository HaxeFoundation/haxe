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
package tools;

class Unserializer {

	var buf : String;
	var pos : Int;
	var length : Int;
	var cache : Array<Dynamic>;

	public function new( buf : String ) {
		this.buf = buf;
		length = buf.length;
		pos = 0;
		cache = new Array();
	}

	function readDigits() {
		var k = 0;
		var s = false;
		while( true ) {
			var c = buf.charCodeAt(pos) - 48;
			if( c == -3 ) {
				s = true;
				pos++;
				continue;
			}
			if( c < 0 || c >= 10 )
				break;
			k = k * 10 + c;
			pos++;
		}
		if( s )
			k *= -1;
		return k;
	}

	public function unserialize() : Dynamic {
		switch( buf.charCodeAt(pos++) ) {
		case 110: // n
			return null;
		case 116: // t
			return true;
		case 102: // f
			return false;
		case 122: // z
			return 0;
		case 105: // i
			return readDigits();
		case 100: // d
			var p1 = pos;
			while( true ) {
				var c = buf.charCodeAt(pos);
				// + - . , 0-9
				if( (c >= 43 && c < 58) || c == 101 /*e*/ || c == 69 /*E*/ )
					pos++;
				else
					break;
			}
			var s = buf.substr(p1,pos-p1);
			var f = Std.parseFloat(s);
			if( f == null )
				throw ("Invalid float "+s);
			return f;
		case 107: // k
			return Std.nan;
		case 109: // m
			return -Std.infinity;
		case 112: // p
			return Std.infinity;
		case 115: // s
			var len = readDigits();
			if( buf.charAt(pos++) == ":" ) {
				if( length - pos < len )
					throw "Invalid string length";
				var s = buf.substr(pos,len);
				pos += len;
				s = s.split("\\\"").join("\"").split("\\r").join("\r").split("\n").join("\\n").split("\\\\").join("\\");
				cache.push(s);
				return s;
			}
		case 97: // a
			var a = new Array<Dynamic>();
			cache.push(a);
			while( true ) {
				if( pos >= length )
					throw "Invalid array";
				var c = buf.charCodeAt(pos);
				if( c == 104 ) /*h*/
					break;
				if( c == 117 ) { /*u*/
					var n = readDigits();
					if( n <= 0 )
						throw "Invalid array nulls";
					a[a.length+n-1] = null;
				} else
					a.push(unserialize());
			}
			pos++;
			return a;
		case 111: // o
			var o = Reflect.empty();
			cache.push(o);
			while( true ) {
				if( pos >= length )
					throw "Invalid object";
				if( buf.charCodeAt(pos) == 103 ) /*g*/
					break;
				var k = unserialize();
				if( !Std.is(k,String) )
					throw "Invalid object key";
				var v = unserialize();
				Reflect.setField(o,k,v);
			}
			pos++;
			return o;
		case 114: // r
			var n = readDigits();
			if( n < 0 || n >= cache.length )
				throw "Invalid reference";
			return cache[n];
		default:
		}
		pos--;
		throw ("Invalid char "+buf.charAt(pos)+" at position "+pos);
	}

}

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
		a : array
		u : array nulls
		h : array end
		o : object
		g : object end
		r : reference
	*/

	function serializeString( s : String ) {
		if( serializeRef(s) )
			return;
		s = s.split("\\").join("\\\\").split("\n").join("\\n").split("\r").join("\\r").split("\"").join("\\\"");
		buf.add("s");
		buf.add(s.length);
		buf.add(":");
		buf.add(s);
	}

	function serializeRef(v) {
		for( i in 0...cache.length ) {
			if( cache[i] == v ) {
				buf.add("r");
				buf.add(i);
				return true;
			}
		}
		cache.push(v);
		return false;
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
			if( Std.isNaN(v) )
				buf.add("k");
			else if( !Std.isFinite(v) )
				buf.add(if( v < 0 ) "m" else "p");
			else {
				buf.add("d");
				buf.add(Std.string(v));
			}
			return;
		}
		if( Std.is(v,String) ) {
			serializeString(v);
			return;
		}
		if( Std.is(v,Array) ) {
			if( serializeRef(v) )
				return;
			var ucount = 0;
			buf.add("a");
			for( i in 0...v.length ) {
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
				buf.add("u");
				buf.add(ucount);
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

		buf.add("o");
		var fl = Reflect.fields(v);
		for( f in fl ) {
			serializeString(f);
			serialize(Reflect.field(v,f));
		}
		buf.add("g");
	}

}

class JsProxy implements Dynamic<JsProxy> {

	var path : Array<String>;

	public function new() {
		path = new Array();
	}

	public function eval( result : Dynamic -> Void ) {		
		#if flash8
		untyped flash.external.ExternalInterface._initJS();
		var v = untyped flash.external.ExternalInterface._evalJS("tools.JsProxy.__eval(\""+path.join(".")+"\")");
		result(unserialize(v));
		#else js
		throw "Unimplemented";
		#else error
		#end		
	}

	public function call( args : Array<Dynamic>, result : Dynamic -> Void ) : Void {
		var s = new Serializer();
		s.serialize(path);
		s.serialize(args);
		var data = s.toString();
		#if flash8
		untyped flash.external.ExternalInterface._initJS();
		var code = data.split("\\").join("\\\\").split("\"").join("\\\"");
		var v = untyped flash.external.ExternalInterface._evalJS("tools.JsProxy.__call(\""+code+"\")");
		result(unserialize(v));
		#else js
		throw "Unimplemented";
		#else error
		#end
	}

	function __resolve(field) {
		var s = new JsProxy();
		s.path = path.copy();
		s.path.push(field);
		return s;
	}

	static function unserialize(v : String) : Dynamic {
		var exc = (v.charAt(0) == "x");
		var u = new Unserializer(if( exc ) v.substr(1,v.length-1) else v);
		var v = u.unserialize();
		if( exc )
			throw v;
		return v;
	}

#if js
	static function __call( code : String ) : String {
		var s = new Unserializer(code);
		var p = s.unserialize();
		var args = s.unserialize();
		try {
			var m : String = p.pop();
			var opath = p.join(".");
			var o = js.Lib.eval(opath);
			var f = Reflect.field(o,m);
			if( !Reflect.isFunction(f) )
				throw ("No such method "+opath+"."+m);
			var s = new Serializer();
			s.serialize(Reflect.callMethod(o,f,args));
			return s.toString();
		} catch( e : Dynamic ) {
			var s = new Serializer();
			s.serialize(e);
			return "x"+s.toString();
		}
	}

	static function __eval( path : String ) : String {
		try {
			var s = new Serializer();
			var v = js.Lib.eval(path);
			s.serialize(v);
			return s.toString();
		} catch( e : Dynamic ) {
			var s = new Serializer();
			s.serialize(e);
			return "x"+s.toString();
		}
	}
#end

}

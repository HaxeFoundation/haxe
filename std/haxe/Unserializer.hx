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
import Reflect;

signature TypeResolver {
	resolveClass : Array<String> -> Class,
	resolveEnum : Array<String> -> Dynamic
}

class Unserializer {

	public static var DEFAULT_RESOLVER : TypeResolver = Reflect;

 	var buf : String;
 	var pos : Int;
 	var length : Int;
 	var cache : Array<Dynamic>;
 	var resolver : TypeResolver;

 	public function new( buf : String ) {
 		this.buf = buf;
 		length = buf.length;
 		pos = 0;
 		cache = new Array();
 		setResolver(DEFAULT_RESOLVER);
 	}

 	public function setResolver( r ) {
		if( r == null )
			resolver = {
				resolveClass : function(_) { return null; },
				resolveEnum : function(_) { return null; }
			};
		else
			resolver = r;
	}

 	function readDigits() {
 		var k = 0;
 		var s = false;
 		var fpos = pos;
 		while( true ) {
 			var c = buf.charCodeAt(pos);
 			if( c == null )
 				break;
 			if( c == 45 ) { // negative sign
 				if( pos != fpos )
 					break;
 				s = true;
 				pos++;
 				continue;
 			}
			c -= 48;
 			if( c < 0 || c > 9 )
 				break;
 			k = k * 10 + c;
 			pos++;
 		}
 		if( s )
 			k *= -1;
 		return k;
 	}

	function unserializeObject() : Dynamic {
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
 			return Math.NaN;
 		case 109: // m
 			return Math.NEGATIVE_INFINITY;
 		case 112: // p
 			return Math.POSITIVE_INFINITY;
 		case 115: // s
 			var len = readDigits();
 			if( buf.charAt(pos++) != ":" || length - pos < len )
				throw "Invalid string length";
 			var s = buf.substr(pos,len);
 			pos += len;
			cache.push(s);
			return s;
 		case 106: // j
 			var len = readDigits();
 			if( buf.charAt(pos++) != ":" )
 				throw "Invalid string length";
 			#if neko
			if( length - pos < len )
				throw "Invalid string length";
 			var s = buf.substr(pos,len);
 			pos += len;
 			#else true
 			var old = pos;
 			var max = pos + len;
 			while( pos < max ) {
				var c = buf.charCodeAt(pos++);
				if( c < 0x7F )
					continue;
				if( c < 0x7FF ) {
					max--;
					continue;
				}
				if( c < 0xFFFF ) {
					max -= 2;
					continue;
				}
				max -= 3;
			}
			len = max - old;
			if( pos != max || length - old < len )
				throw "Invalid string length";
			var s = buf.substr(old,len);
 			#end
 			s = s.split("\\r").join("\r").split("\\n").join("\n").split("\\\\").join("\\");
 			cache.push(s);
 			return s;
 		case 97: // a
 			var a = new Array<Dynamic>();
 			cache.push(a);
 			while( true ) {
 				if( pos >= length )
 					throw "Invalid array";
 				var c = buf.charCodeAt(pos);
 				if( c == 104 ) { /*h*/
					pos++;
 					break;
				}
 				if( c == 117 ) { /*u*/
					pos++;
 					var n = readDigits();
 					if( n <= 0 )
 						throw "Invalid array null counter";
 					a[a.length+n-1] = null;
 				} else
 					a.push(unserialize());
 			}
 			return a;
 		case 111: // o
			return unserializeObject();
 		case 114: // r
 			var n = readDigits();
 			if( n < 0 || n >= cache.length )
 				throw "Invalid reference";
 			return cache[n];
 		case 120: // x
			throw unserialize();
		case 99: // c
			var a : Array<String> = unserialize();
            if( !Std.is(a,Array) )
				throw "Invalid class name";
			for(s in a)
				if( !Std.is(s,String) )
					throw "Invalid class name";
			var cl = resolver.resolveClass(a);
			if( cl == null )
				throw "Class not found " + a.join(".");
			var o = unserializeObject();
			Reflect.setPrototype(o,cl.prototype);
			return o;
		case 119: // w
			var a : Array<String> = unserialize();
            if( !Std.is(a,Array) )
				throw "Invalid enum name";
			for(s in a)
				if( !Std.is(s,String) )
					throw "Invalid enum name";
			var e = resolver.resolveEnum(a);
			if( e == null )
				throw "Enum not found " + a.join(".");
			var tag = unserialize();
			if( !Std.is(tag,String) )
				throw "Invalid enum tag";
			var constr = Reflect.field(e,tag);
			if( constr == null )
				throw "Unknown enum tag "+a.join(".")+"."+tag;
			var nargs = readDigits();
			var args = null;
			if( nargs > 0 ) {
				args = new Array();
				while( nargs > 0 ) {
					args.push(unserialize());
					nargs -= 1;
				}
			}
			#if neko
				var v = {
					tag : untyped tag.__s,
					__string : function() { return untyped neko.Boot.__enum_str(this); },
					__enum__ : e
				};
				if( args != null )
					untyped v.args = args.__a;
				return v;
			#else true
				if( args == null )
					args = [tag];
				else
					args.unshift(tag);
				untyped args.__enum__ = e;
				return args;
			#end
 		default:
 		}
 		pos--;
 		throw ("Invalid char "+buf.charAt(pos)+" at position "+pos);
 	}

	/**
		Unserialize a single value and return it.
	**/
	public static function run( v : String ) {
		return new Unserializer(v).unserialize();
	}

}

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

	/**
		If the values you are serializing can contain
		circular references or objects repetitions, you should
		set USE_CACHE to true to prevent infinite loops.
	**/
	public static var USE_CACHE = false;

	/**
		Use constructor indexes for enums instead of names.
		This is less reliable but more compact.
	**/
	public static var USE_ENUM_INDEX = false;

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789%:";

	var buf : StringBuf;
	var cache : Array<Dynamic>;
	var shash : Hash<Int>;
	var scount : Int;
	public var useCache : Bool;
	public var useEnumIndex : Bool;

	public function new() {
		buf = new StringBuf();
		cache = new Array();
		useCache = USE_CACHE;
		useEnumIndex = USE_ENUM_INDEX;
		shash = new Hash();
		scount = 0;
	}

	public function toString() {
		return buf.toString();
	}

	/* prefixes :
		a : array
		b : hash
		c : class
		d : Float
		e : reserved (float exp)
		f : false
		g : object end
		h : array/list/hash end
		i : Int
		j : enum (by index)
		k : NaN
		l : list
		m : -Inf
		n : null
		o : object
		p : +Inf
		q : inthash
		r : reference
		s : bytes (base64)
		t : true
		u : array nulls
		v : date
		w : enum
		x : exception
		y : urlencoded string
		z : zero
	*/

	function serializeString( s : String ) {
		var x = shash.get(s);
		if( x != null ) {
			buf.add("R");
			buf.add(x);
			return;
		}
		shash.set(s,scount++);
		#if old_serialize
			// no more support for -D old_serialize due to 'j' reuse
			#if error #end
		#end
		buf.add("y");
		s = StringTools.urlEncode(s);
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
			#else
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

	#if flash9
	// only the instance variables

	function serializeClassFields(v,c) {
		var xml : flash.xml.XML = untyped __global__["flash.utils.describeType"](c);
		var vars = xml.factory[0].child("variable");
		for( i in 0...vars.length() ) {
			var f = vars[i].attribute("name").toString();
			if( !v.hasOwnProperty(f) )
				continue;
			serializeString(f);
			serialize(Reflect.field(v,f));
		}
		buf.add("g");
	}
	#end

	function serializeFields(v) {
		for( f in Reflect.fields(v) ) {
			serializeString(f);
			serialize(Reflect.field(v,f));
		}
		buf.add("g");
	}

	public function serialize( v : Dynamic ) {
		switch( Type.typeof(v) ) {
		case TNull:
			buf.add("n");
		case TInt:
			if( v == 0 ) {
				buf.add("z");
				return;
			}
			buf.add("i");
			buf.add(v);
		case TFloat:
			if( Math.isNaN(v) )
				buf.add("k");
			else if( !Math.isFinite(v) )
				buf.add(if( v < 0 ) "m" else "p");
			else {
				buf.add("d");
				buf.add(v);
			}
		case TBool:
			buf.add(if( v ) "t" else "f");
		case TClass(c):
			if( c == String ) {
				serializeString(v);
				return;
			}
			if( useCache && serializeRef(v) )
				return;
			switch( c ) {
			case cast Array:
				var ucount = 0;
				buf.add("a");
				#if flash9
				var v : Array<Dynamic> = v;
				#end
				var l = #if (neko || flash9 || php) v.length #else v[untyped "length"] #end;
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
			case cast List:
				buf.add("l");
				var v : List<Dynamic> = v;
				for( i in v )
					serialize(i);
				buf.add("h");
			case cast Date:
				var d : Date = v;
				buf.add("v");
				buf.add(d.toString());
			case cast Hash:
				buf.add("b");
				var v : Hash<Dynamic> = v;
				for( k in v.keys() ) {
					serializeString(k);
					serialize(v.get(k));
				}
				buf.add("h");
			case cast IntHash:
				buf.add("q");
				var v : IntHash<Dynamic> = v;
				for( k in v.keys() ) {
					buf.add(":");
					buf.add(k);
					serialize(v.get(k));
				}
				buf.add("h");
			case cast haxe.io.Bytes:
				var v : haxe.io.Bytes = v;
				#if neko
				var chars = new String(base_encode(v.getData(),untyped BASE64.__s));
				#else
				var i = 0;
				var max = v.length - 2;
				var chars = "";
				var b64 = BASE64;
				while( i < max ) {
					var b1 = v.get(i++);
					var b2 = v.get(i++);
					var b3 = v.get(i++);
					chars += b64.charAt(b1 >> 2)
						+ b64.charAt(((b1 << 4) | (b2 >> 4)) & 63)
						+ b64.charAt(((b2 << 2) | (b3 >> 6)) & 63)
						+ b64.charAt(b3 & 63);
				}
				if( i == max ) {
					var b1 = v.get(i++);
					var b2 = v.get(i++);
					chars += b64.charAt(b1 >> 2)
						+ b64.charAt(((b1 << 4) | (b2 >> 4)) & 63)
						+ b64.charAt((b2 << 2) & 63);
				} else if( i == max + 1 ) {
					var b1 = v.get(i++);
					chars += b64.charAt(b1 >> 2) + b64.charAt((b1 << 4) & 63);
				}
				#end
				buf.add("s");
				buf.add(chars.length);
				buf.add(":");
				buf.add(chars);
			default:
				cache.pop();
				buf.add("c");
				serializeString(Type.getClassName(c));
				cache.push(v);
				#if flash9
				serializeClassFields(v,c);
				#else
				serializeFields(v);
				#end
			}
		case TObject:
			if( useCache && serializeRef(v) )
				return;
			buf.add("o");
			serializeFields(v);
		case TEnum(e):
			if( useCache && serializeRef(v) )
				return;
			cache.pop();
			buf.add(useEnumIndex?"j":"w");
			serializeString(Type.getEnumName(e));
			#if neko
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v.index);
			} else
				serializeString(new String(v.tag));
			buf.add(":");
			if( v.args == null )
				buf.add(0);
			else {
				var l : Int = untyped __dollar__asize(v.args);
				buf.add(l);
				for( i in 0...l )
					serialize(v.args[i]);
			}
			#elseif flash9
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v.index);
			} else
				serializeString(v.tag);
			buf.add(":");
			var pl : Array<Dynamic> = v.params;
			if( pl == null )
				buf.add(0);
			else {
				buf.add(pl.length);
				for( p in pl )
					serialize(p);
			}
			#elseif php
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v.index);
			} else
				serializeString(v.tag);
			buf.add(":");
			var l : Int = untyped __call__("count", v.params);
			if( l == 0 || v.params == null)
				buf.add(0);
			else {
				buf.add(l);
				for( i in 0...l )
					serialize(untyped __field__(v, __php__("params"), i));
			}
			#else
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v[1]);
			} else
				serializeString(v[0]);
			buf.add(":");
			var l = v[untyped "length"];
			buf.add(l - 2);
			for( i in 2...l )
				serialize(v[i]);
			#end
			cache.push(v);
		case TFunction:
			throw "Cannot serialize function";
		default:
			throw "Cannot serialize "+Std.string(v);
		}
	}

	public function serializeException( e : Dynamic ) {
		buf.add("x");
		#if flash9
		if( untyped __is__(e,__global__["Error"]) ) {
			var s = e.getStackTrace();
			if( s == null )
				serialize(e.message);
			else
				serialize(s);
			return;
		}
		#end
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

	#if neko
	static var base_encode = neko.Lib.load("std","base_encode",2);
	#end

}


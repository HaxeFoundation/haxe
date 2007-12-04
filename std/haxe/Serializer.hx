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

	var buf : StringBuf;
	var cache : Array<Dynamic>;
	var shash : Hash<Int>;
	var scount : Int;
	var useCache : Bool;
	var useEnumIndex : Bool;

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
		s :
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

	#if flash9
	// only the instance variables

	function serializeClassFields(v,c) {
		var xml : Dynamic = untyped __global__["flash.utils.describeType"](c).factory;
		var vars = xml.child("variable");
		for( i in 0...vars.length() ) {
			var f = untyped vars[i].attribute("name").toString();
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
			#if flash9
			case cast flash.utils.ByteArray:
				buf.add("y");
				var s = "";
				var b : flash.utils.ByteArray = v;
				var CHARS = ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"];
				for( p in 0...b.length ) {
					var c = b[p];
					// 0-9a-zA-Z
					if( (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) )
						s += String.fromCharCode(c);
					else
						s += "%"+CHARS[c>>4]+CHARS[c&15];
				}
				buf.add(s.length);
				buf.add(":");
				buf.add(s);
			#end
			default:
				cache.pop();
				buf.add("c");
				serializeString(Type.getClassName(c));
				cache.push(v);
				#if flash9
				serializeClassFields(v,c);
				#else true
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
			#else flash9
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v.index);
			} else
				serializeString(v.tag);
			buf.add(":");
			if( v.params == null )
				buf.add(0);
			else {
				var l : Int = v.params.length;
				buf.add(l);
				for( i in 0...l )
					serialize(v.params[i]);
			}
			#else true
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

}


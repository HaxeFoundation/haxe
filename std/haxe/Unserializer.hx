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
package haxe;

@:noDoc
typedef TypeResolver = {
	function resolveClass( name : String ) : Class<Dynamic>;
	function resolveEnum( name : String ) : Enum<Dynamic>;
}

/**
	The `Unserializer` class is the complement to the `Serializer` class. It parses
	a serialization `String` and creates objects from the contained data.

	This class can be used in two ways:

	- create a `new Unserializer()` instance with a given serialization
		String, then call its `unserialize()` method until all values are
		extracted
	- call `Unserializer.run()`  to unserialize a single value from a given
		String

	The specification of the serialization format can be found here:
	<https://haxe.org/manual/serialization/format>
**/
class Unserializer {

	/**
		This value can be set to use custom type resolvers.

		A type resolver finds a `Class` or `Enum` instance from a given `String`. 
		By default, the Haxe `Type` Api is used.

		A type resolver must provide two methods:

		1. `resolveClass(name:String):Class<Dynamic>` is called to determine a
				`Class` from a class name
		2. `resolveEnum(name:String):Enum<Dynamic>` is called to determine an
				`Enum` from an enum name

		This value is applied when a new `Unserializer` instance is created.
		Changing it afterwards has no effect on previously created instances.
	**/
	public static var DEFAULT_RESOLVER : TypeResolver = new DefaultResolver();

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789%:";

	#if !neko
	static var CODES = null;

	static function initCodes() {
		var codes =
			#if flash
				new flash.utils.ByteArray();
			#else
				new Array();
			#end
		for( i in 0...BASE64.length )
			codes[StringTools.fastCodeAt(BASE64,i)] = i;
		return codes;
	}
	#end

 	var buf : String;
 	var pos : Int;
 	var length : Int;
 	var cache : Array<Dynamic>;
 	var scache : Array<String>;
 	var resolver : TypeResolver;
 	#if neko
 	var upos : Int;
 	#end

	/**
		Creates a new Unserializer instance, with its internal buffer
		initialized to `buf`.

		This does not parse `buf` immediately. It is parsed only when calls to
		`this.unserialize` are made.

		Each Unserializer instance maintains its own cache.
	**/
 	public function new( buf : String ) {
 		this.buf = buf;
 		length = buf.length;
 		pos = 0;
 		#if neko
 		upos = 0;
 		#end
 		scache = new Array();
 		cache = new Array();
		var r = DEFAULT_RESOLVER;
		if( r == null ) {
			r = new DefaultResolver();
			DEFAULT_RESOLVER = r;
		}
		resolver = r;
 	}

	/**
		Sets the type resolver of `this` Unserializer instance to `r`.

		If `r` is null, a special resolver is used which returns null for all
		input values.

		See `DEFAULT_RESOLVER` for more information on type resolvers.
	**/
 	public function setResolver( r ) {
		if( r == null )
			resolver = NullResolver.instance;
		else
			resolver = r;
	}

	/**
		Gets the type resolver of `this` Unserializer instance.

		See `DEFAULT_RESOLVER` for more information on type resolvers.
	**/
 	public function getResolver() {
		return resolver;
	}

	inline function get(p) : Int {
		return StringTools.fastCodeAt(buf, p);
	}

 	function readDigits() {
 		var k = 0;
 		var s = false;
 		var fpos = pos;
 		while( true ) {
 			var c = get(pos);
			if( StringTools.isEof(c) )
				break;
 			if( c == "-".code ) {
 				if( pos != fpos )
 					break;
 				s = true;
 				pos++;
 				continue;
 			}
 			if( c < "0".code || c > "9".code )
 				break;
 			k = k * 10 + (c - "0".code);
 			pos++;
 		}
 		if( s )
 			k *= -1;
 		return k;
 	}

	function readFloat() {
		var p1 = pos;
 		while( true ) {
 			var c = get(pos);
			if( StringTools.isEof(c)) break;
 			// + - . , 0-9
 			if( (c >= 43 && c < 58) || c == "e".code || c == "E".code )
 				pos++;
 			else
 				break;
 		}
 		return Std.parseFloat(buf.substr(p1,pos-p1));
	}

	function unserializeObject(o) {
 		while( true ) {
 			if( pos >= length )
 				throw "Invalid object";
 			if( get(pos) == "g".code )
 				break;
 			var k = unserialize();
 			if( !Std.is(k,String) )
 				throw "Invalid object key";
 			var v = unserialize();
 			Reflect.setField(o,k,v);
 		}
 		pos++;
	}

	function unserializeEnum( edecl, tag ) {
		if( get(pos++) != ":".code )
			throw "Invalid enum format";
		var nargs = readDigits();
		if( nargs == 0 )
			return Type.createEnum(edecl,tag);
		var args = new Array();
		while( nargs-- > 0 )
			args.push(unserialize());
		return Type.createEnum(edecl,tag,args);
	}

	/**
		Unserializes the next part of `this` Unserializer instance and returns
		the according value.

		This function may call `this.resolver.resolveClass` to determine a
		Class from a String, and `this.resolver.resolveEnum` to determine an
		Enum from a String.

		If `this` Unserializer instance contains no more or invalid data, an
		exception is thrown.

		This operation may fail on structurally valid data if a type cannot be
		resolved or if a field cannot be set. This can happen when unserializing
		Strings that were serialized on a different Haxe target, in which the
		serialization side has to make sure not to include platform-specific
		data.

		Classes are created from `Type.createEmptyInstance`, which means their
		constructors are not called.
	**/
 	public function unserialize() : Dynamic {
 		switch( get(pos++) ) {
 		case "n".code:
 			return null;
 		case "t".code:
 			return true;
 		case "f".code:
 			return false;
 		case "z".code:
 			return 0;
 		case "i".code:
 			return readDigits();
 		case "d".code:
 			return readFloat();
		case "y".code:
 			var len = readDigits();
 			if( get(pos++) != ":".code || length - pos < len )
 				throw "Invalid string length";
			var s = buf.substr(pos,len);
			pos += len;
			s = StringTools.urlDecode(s);
			scache.push(s);
			return s;
 		case "k".code:
 			return Math.NaN;
 		case "m".code:
 			return Math.NEGATIVE_INFINITY;
 		case "p".code:
 			return Math.POSITIVE_INFINITY;
 		case "a".code:
			var buf = buf;
 			var a = new Array<Dynamic>();
 			#if cpp var cachePos = cache.length; #end
 			cache.push(a);
 			while( true ) {
 				var c = get(pos);
 				if( c == "h".code ) {
					pos++;
 					break;
				}
 				if( c == "u".code ) {
					pos++;
 					var n = readDigits();
 					a[a.length+n-1] = null;
 				} else
 					a.push(unserialize());
 			}
 			#if cpp
 			return cache[cachePos] = cpp.NativeArray.resolveVirtualArray(a);
 			#else
 			return a;
 			#end
 		case "o".code:
	 		var o = {};
	 		cache.push(o);
			unserializeObject(o);
			return o;
 		case "r".code:
 			var n = readDigits();
 			if( n < 0 || n >= cache.length )
 				throw "Invalid reference";
 			return cache[n];
 		case "R".code:
			var n = readDigits();
			if( n < 0 || n >= scache.length )
				throw "Invalid string reference";
			return scache[n];
 		case "x".code:
			throw unserialize();
		case "c".code:
	 		var name = unserialize();
			var cl = resolver.resolveClass(name);
			if( cl == null )
				throw "Class not found " + name;
			var o = Type.createEmptyInstance(cl);
			cache.push(o);
			unserializeObject(o);
			return o;
		case "w".code:
			var name = unserialize();
			var edecl = resolver.resolveEnum(name);
			if( edecl == null )
				throw "Enum not found " + name;
			var e = unserializeEnum(edecl, unserialize());
			cache.push(e);
			return e;
 		case "j".code:
			var name = unserialize();
			var edecl = resolver.resolveEnum(name);
			if( edecl == null )
				throw "Enum not found " + name;
			pos++; /* skip ':' */
			var index = readDigits();
			var tag = Type.getEnumConstructs(edecl)[index];
			if( tag == null )
				throw "Unknown enum index "+name+"@"+index;
			var e = unserializeEnum(edecl, tag);
			cache.push(e);
			return e;
		case "l".code:
			var l = new List();
			cache.push(l);
			var buf = buf;
			while( get(pos) != "h".code )
				l.add(unserialize());
			pos++;
			return l;
		case "b".code:
			var h = new haxe.ds.StringMap();
			cache.push(h);
			var buf = buf;
			while( get(pos) != "h".code ) {
				var s = unserialize();
				h.set(s,unserialize());
			}
			pos++;
			return h;
		case "q".code:
			var h = new haxe.ds.IntMap();
			cache.push(h);
			var buf = buf;
			var c = get(pos++);
			while( c == ":".code ) {
				var i = readDigits();
				h.set(i,unserialize());
				c = get(pos++);
			}
			if( c != "h".code )
				throw "Invalid IntMap format";
			return h;
		case "M".code:
			var h = new haxe.ds.ObjectMap();
			cache.push(h);
			var buf = buf;
			while( get(pos) != "h".code ) {
				var s = unserialize();
				h.set(s,unserialize());
			}
			pos++;
			return h;
		case "v".code:
			var d;
			if(	get(pos) >= '0'.code && get(pos) <= '9'.code &&
				get(pos + 1) >= '0'.code && get(pos + 1) <= '9'.code &&
				get(pos + 2) >= '0'.code && get(pos + 2) <= '9'.code &&
				get(pos + 3) >= '0'.code && get(pos + 3) <= '9'.code &&
				get(pos + 4) == '-'.code
				) {
				// Included for backwards compatibility
				d = Date.fromString(buf.substr(pos,19));
				pos += 19;
			} else
				d = Date.fromTime(readFloat());
			cache.push(d);
			return d;
 		case "s".code:
 			var len = readDigits();
			var buf = buf;
 			if( get(pos++) != ":".code || length - pos < len )
				throw "Invalid bytes length";
			#if neko
			var bytes = haxe.io.Bytes.ofData( base_decode(untyped buf.substr(pos,len).__s,untyped BASE64.__s) );
			#else
			var codes = CODES;
			if( codes == null ) {
				codes = initCodes();
				CODES = codes;
			}
			var i = pos;
			var rest = len & 3;
			var size = (len >> 2) * 3 + ((rest >= 2) ? rest - 1 : 0);
			var max = i + (len - rest);
			var bytes = haxe.io.Bytes.alloc(size);
			var bpos = 0;
			while( i < max ) {
				var c1 = codes[StringTools.fastCodeAt(buf,i++)];
				var c2 = codes[StringTools.fastCodeAt(buf,i++)];
				bytes.set(bpos++,(c1 << 2) | (c2 >> 4));
				var c3 = codes[StringTools.fastCodeAt(buf,i++)];
				bytes.set(bpos++,(c2 << 4) | (c3 >> 2));
				var c4 = codes[StringTools.fastCodeAt(buf,i++)];
				bytes.set(bpos++,(c3 << 6) | c4);
			}
			if( rest >= 2 ) {
				var c1 = codes[StringTools.fastCodeAt(buf,i++)];
				var c2 = codes[StringTools.fastCodeAt(buf,i++)];
				bytes.set(bpos++,(c1 << 2) | (c2 >> 4));
				if( rest == 3 ) {
					var c3 = codes[StringTools.fastCodeAt(buf,i++)];
					bytes.set(bpos++,(c2 << 4) | (c3 >> 2));
				}
			}
 			#end
			pos += len;
			cache.push(bytes);
			return bytes;
		case "C".code:
	 		var name = unserialize();
			var cl = resolver.resolveClass(name);
			if( cl == null )
				throw "Class not found " + name;
			var o : Dynamic = Type.createEmptyInstance(cl);
			cache.push(o);
			o.hxUnserialize(this);
			if( get(pos++) != "g".code )
				throw "Invalid custom data";
			return o;
		case "A".code:
			var name = unserialize();
			var cl = resolver.resolveClass(name);
			if( cl == null )
				throw "Class not found " + name;
			return cl;
		case "B".code:
			var name = unserialize();
			var e = resolver.resolveEnum(name);
			if( e == null )
				throw "Enum not found " + name;
			return e;
 		default:
 		}
 		pos--;
 		throw ("Invalid char "+buf.charAt(pos)+" at position "+pos);
 	}

	/**
		Unserializes `v` and returns the according value.

		This is a convenience function for creating a new instance of
		Unserializer with `v` as buffer and calling its unserialize() method
		once.
	**/
	public static function run( v : String ) : Dynamic {
		return new Unserializer(v).unserialize();
	}

	#if neko
	static var base_decode = neko.Lib.load("std","base_decode",2);
	#end

}

private class DefaultResolver {
	public function new() {}
	@:final public inline function resolveClass(name:String):Class<Dynamic> return Type.resolveClass(name);
	@:final public inline function resolveEnum(name:String):Enum<Dynamic> return Type.resolveEnum(name);
}

private class NullResolver {
	function new() {}
	@:final public inline function resolveClass(name:String):Class<Dynamic> return null;
	@:final public inline function resolveEnum(name:String):Enum<Dynamic> return null;
	public static var instance(get,null):NullResolver;
	inline static function get_instance():NullResolver {
		if (instance == null) instance = new NullResolver();
		return instance;
	}
}

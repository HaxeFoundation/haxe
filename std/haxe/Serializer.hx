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

/**
	The Serializer class can be used to encode values and objects into a `String`,
	from which the `Unserializer` class can recreate the original representation.

	This class can be used in two ways:

	- create a `new Serializer()` instance, call its `serialize()` method with
		any argument and finally retrieve the String representation from
		`toString()`
	- call `Serializer.run()` to obtain the serialized representation of a
		single argument

	Serialization is guaranteed to work for all haxe-defined classes, but may
	or may not work for instances of external/native classes.

	The specification of the serialization format can be found here:
	<https://haxe.org/manual/serialization/format>
**/
class Serializer {

	/**
		If the values you are serializing can contain circular references or
		objects repetitions, you should set `USE_CACHE` to true to prevent
		infinite loops.

		This may also reduce the size of serialization Strings at the expense of
		performance.

		This value can be changed for individual instances of Serializer by
		setting their useCache field.
	**/
	public static var USE_CACHE = false;

	/**
		Use constructor indexes for enums instead of names.

		This may reduce the size of serialization Strings, but makes them less
		suited for long-term storage: If constructors are removed or added from
		the enum, the indices may no longer match.

		This value can be changed for individual instances of Serializer by
		setting their useEnumIndex field.
	**/
	public static var USE_ENUM_INDEX = false;

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789%:";
	static var BASE64_CODES = null;

	var buf : StringBuf;
	var cache : Array<Dynamic>;
	var shash : haxe.ds.StringMap<Int>;
	var scount : Int;

	/**
		The individual cache setting for `this` Serializer instance.

		See USE_CACHE for a complete description.
	**/
	public var useCache : Bool;

	/**
		The individual enum index setting for `this` Serializer instance.

		See USE_ENUM_INDEX for a complete description.
	**/
	public var useEnumIndex : Bool;

	/**
		Creates a new Serializer instance.

		Subsequent calls to `this.serialize` will append values to the
		internal buffer of this String. Once complete, the contents can be
		retrieved through a call to `this.toString`.

		Each Serializer instance maintains its own cache if this.useCache` is
		true.
	**/
	public function new() {
		buf = new StringBuf();
		cache = new Array();
		useCache = USE_CACHE;
		useEnumIndex = USE_ENUM_INDEX;
		shash = new haxe.ds.StringMap();
		scount = 0;
	}

	/**
		Return the String representation of `this` Serializer.

		The exact format specification can be found here:
		https://haxe.org/manual/serialization/format
	**/
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
		q : haxe.ds.IntMap
		r : reference
		s : bytes (base64)
		t : true
		u : array nulls
		v : date
		w : enum
		x : exception
		y : urlencoded string
		z : zero
		A : Class<Dynamic>
		B : Enum<Dynamic>
		M : haxe.ds.ObjectMap
		C : custom
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

	#if flash
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

	/**
		Serializes `v`.

		All haxe-defined values and objects with the exception of functions can
		be serialized. Serialization of external/native objects is not
		guaranteed to work.

		The values of `this.useCache` and `this.useEnumIndex` may affect
		serialization output.
	**/
	public function serialize( v : Dynamic ) {
		switch( Type.typeof(v) ) {
		case TNull:
			buf.add("n");
		case TInt:
			var v : Int = v;
			if( v == 0 ) {
				buf.add("z");
				return;
			}
			buf.add("i");
			buf.add(v);
		case TFloat:
			var v : Float = v;
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
			if( #if neko untyped c.__is_String #else c == String #end ) {
				serializeString(v);
				return;
			}
			if( useCache && serializeRef(v) )
				return;
			switch( #if (neko || cs || python) Type.getClassName(c) #else c #end ) {
			case #if (neko || cs || python) "Array" #else cast Array #end:
				var ucount = 0;
				buf.add("a");
				#if (flash || python || hl)
				var v : Array<Dynamic> = v;
				#end
				var l = #if (neko || flash || php || cs || java || python || hl || lua) v.length #elseif cpp v.__length() #else __getField(v, "length") #end;
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
			case #if (neko || cs || python) "List" #else cast List #end:
				buf.add("l");
				var v : List<Dynamic> = v;
				for( i in v )
					serialize(i);
				buf.add("h");
			case #if (neko || cs || python) "Date" #else cast Date #end:
				var d : Date = v;
				buf.add("v");
				buf.add(d.getTime());
			case #if (neko || cs || python) "haxe.ds.StringMap" #else cast haxe.ds.StringMap #end:
				buf.add("b");
				var v : haxe.ds.StringMap<Dynamic> = v;
				for( k in v.keys() ) {
					serializeString(k);
					serialize(v.get(k));
				}
				buf.add("h");
			case #if (neko || cs || python) "haxe.ds.IntMap" #else cast haxe.ds.IntMap #end:
				buf.add("q");
				var v : haxe.ds.IntMap<Dynamic> = v;
				for( k in v.keys() ) {
					buf.add(":");
					buf.add(k);
					serialize(v.get(k));
				}
				buf.add("h");
			case #if (neko || cs || python) "haxe.ds.ObjectMap" #else cast haxe.ds.ObjectMap #end:
				buf.add("M");
				var v : haxe.ds.ObjectMap<Dynamic,Dynamic> = v;
				for ( k in v.keys() ) {
					#if (js || neko)
					var id = Reflect.field(k, "__id__");
					Reflect.deleteField(k, "__id__");
					serialize(k);
					Reflect.setField(k, "__id__", id);
					#else
					serialize(k);
					#end
					serialize(v.get(k));
				}
				buf.add("h");
			case #if (neko || cs || python) "haxe.io.Bytes" #else cast haxe.io.Bytes #end:
				var v : haxe.io.Bytes = v;
				#if neko
				var chars = new String(base_encode(v.getData(),untyped BASE64.__s));
				buf.add("s");
				buf.add(chars.length);
				buf.add(":");
				buf.add(chars);
				#else

				buf.add("s");
				buf.add(Math.ceil((v.length * 8) / 6));
				buf.add(":");

				var i = 0;
				var max = v.length - 2;
				var b64 = BASE64_CODES;
				if( b64 == null ) {
					b64 = new haxe.ds.Vector(BASE64.length);
					for( i in 0...BASE64.length )
						b64[i] = BASE64.charCodeAt(i);
					BASE64_CODES = b64;
				}
				while( i < max ) {
					var b1 = v.get(i++);
					var b2 = v.get(i++);
					var b3 = v.get(i++);

					buf.addChar(b64[b1 >> 2]);
					buf.addChar(b64[((b1 << 4) | (b2 >> 4)) & 63]);
					buf.addChar(b64[((b2 << 2) | (b3 >> 6)) & 63]);
					buf.addChar(b64[b3 & 63]);
				}
				if( i == max ) {
					var b1 = v.get(i++);
					var b2 = v.get(i++);
					buf.addChar(b64[b1 >> 2]);
					buf.addChar(b64[((b1 << 4) | (b2 >> 4)) & 63]);
					buf.addChar(b64[(b2 << 2) & 63]);
				} else if( i == max + 1 ) {
					var b1 = v.get(i++);
					buf.addChar(b64[b1 >> 2]);
					buf.addChar(b64[(b1 << 4) & 63]);
				}
				#end
			default:
				if( useCache ) cache.pop();
				if( #if flash try v.hxSerialize != null catch( e : Dynamic ) false #elseif (cs || java || python) Reflect.hasField(v, "hxSerialize") #elseif (php && php7) php.Global.method_exists(v, 'hxSerialize') #else v.hxSerialize != null #end  ) {
					buf.add("C");
					serializeString(Type.getClassName(c));
					if( useCache ) cache.push(v);
					v.hxSerialize(this);
					buf.add("g");
				} else {
					buf.add("c");
					serializeString(Type.getClassName(c));
					if( useCache ) cache.push(v);
					#if flash
					serializeClassFields(v,c);
					#else
					serializeFields(v);
					#end
				}
			}
		case TObject:
			if (Std.is(v,Class)) {
				var className = Type.getClassName(v);
				#if (flash || cpp)
				// Currently, Enum and Class are the same for flash and cpp.
				//  use resolveEnum to test if it is actually an enum
				if (Type.resolveEnum(className)!=null) buf.add("B") else
				#end
				buf.add("A");
				serializeString(className);
			} else if (Std.is(v,Enum)) {
				buf.add("B");
				serializeString(Type.getEnumName(v));
			} else {
				if( useCache && serializeRef(v) )
					return;
				buf.add("o");
				serializeFields(v);
			}
		case TEnum(e):
			if( useCache ) {
				if( serializeRef(v) )
					return;
				cache.pop();
			}
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
			#elseif flash
			if( useEnumIndex ) {
				buf.add(":");
				var i : Int = v.index;
				buf.add(i);
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
			#elseif cpp
			var enumBase:cpp.EnumBase = v;
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(enumBase.getIndex());
			} else
				serializeString(enumBase.getTag());
			buf.add(":");
			var len = enumBase.getParamCount();
			buf.add(len);
			for( p in 0...len )
				serialize( enumBase.getParamI(p));
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
				for( i in 0...l ) {
					#if (php && php7)
					serialize(v.params[i]);
					#elseif php
					serialize(untyped __field__(v, __php__("params"), i));
					#end
				}
			}
			#elseif (java || cs || python || hl)
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(Type.enumIndex(v));
			} else
				serializeString(Type.enumConstructor(v));
			buf.add(":");
			var arr:Array<Dynamic> = Type.enumParameters(v);
			if (arr != null)
			{
				buf.add(arr.length);
				for (v in arr)
					serialize(v);
			} else {
				buf.add("0");
			}

			#else
			if( useEnumIndex ) {
				buf.add(":");
				buf.add(v[1]);
			} else
				serializeString(v[0]);
			buf.add(":");
			var l = __getField(v, "length");
			buf.add(l - 2);
			for( i in 2...l )
				serialize(v[i]);
			#end
			if( useCache ) cache.push(v);
		case TFunction:
			throw "Cannot serialize function";
		default:
			#if neko
			if( untyped (__i32__kind != null && __dollar__iskind(v,__i32__kind)) ) {
				buf.add("i");
				buf.add(v);
				return;
			}
			#end
			throw "Cannot serialize "+Std.string(v);
		}
	}

	@:extern inline function __getField(o:Dynamic, f:String):Dynamic return untyped o[f];

	public function serializeException( e : Dynamic ) {
		buf.add("x");
		#if flash
		if( untyped __is__(e,__global__["Error"]) ) {
			var e : flash.errors.Error = e;
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
		Serializes `v` and returns the String representation.

		This is a convenience function for creating a new instance of
		Serializer, serialize `v` into it and obtain the result through a call
		to toString().
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


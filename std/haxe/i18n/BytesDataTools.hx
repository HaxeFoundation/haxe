package haxe.i18n;

import haxe.io.BytesData;
import haxe.io.Error;

class BytesDataTools {

	#if (!js)

	public static function alloc( length : Int ) : BytesData {
		#if neko
		return untyped __dollar__smake(length);
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.length = length;
		return b;
		#elseif php
		return BytesData.alloc(length);
		#elseif cpp
		var a = new BytesData();
		if (length>0) cpp.NativeArray.setSize(a, length);
		return a;
		#elseif cs
		return new cs.NativeArray(length);
		#elseif java
		return new java.NativeArray(length);
		#elseif python
		return new python.Bytearray(length);
		#else
		var a = new Array();
		for( i in 0...length )
			a.push(0);
		return a;
		#end
	}


	public static inline function get( b:BytesData, pos : Int ) : Int {
		#if neko
		return untyped $sget(b,pos);
		#elseif flash
		return b[pos];
		#elseif php
		return b.get(pos);
		#elseif cpp
		return untyped b[pos];
		#elseif java
		return untyped b[pos] & 0xFF;
		#elseif python
		return python.Syntax.arrayAccess(b, pos);
		#else
		return b[pos];
		#end
	}

	public static inline function set( b:BytesData, pos : Int, v : Int ) : Void {
		#if neko
		untyped $sset(b,pos,v);
		#elseif flash
		b[pos] = v;
		#elseif php
		b.set(pos, v);
		#elseif cpp
		untyped b[pos] = v;
		#elseif java
		b[pos] = cast v;
		#elseif cs
		b[pos] = cast v;
		#elseif python
		python.Syntax.arraySet(b, pos, v & 0xFF);
		#else
		b[pos] = v & 0xFF;
		#end
	}

	public static inline function getLength (b:BytesData):Int {
		#if flash
		return b.length;
		#elseif neko
		return untyped __dollar__ssize(b);
		#elseif php
		return b.length;
		#elseif cs
		return b.Length;
		#else
		return untyped b.length;
		#end
	}


	static public function sub( b:BytesData, pos : Int, len : Int ) : BytesData {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		#end
		#if neko
		return try untyped __dollar__ssub(b,pos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash
		b.position = pos;
		var b2 = new flash.utils.ByteArray();
		b.readBytes(b2,0,len);
		return b2;
		#elseif php
		return b.sub(pos, len);
		#elseif java
		var newarr = new java.NativeArray(len);
		java.lang.System.arraycopy(b, pos, newarr, 0, len);
		return newarr;
		#elseif cs
		var newarr = new cs.NativeArray(len);
		cs.system.Array.Copy(b, pos, newarr, 0, len);
		return newarr;
		#elseif python
		return python.Syntax.arrayAccess(b, pos, pos+len);
		#else
		return b.slice(pos,pos+len);
		#end
	}

	public static function getString( b:BytesData, pos : Int, len : Int ) : String {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		#end
		#if neko
		return try new String(untyped __dollar__ssub(b,pos,len)) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash
		b.position = pos;
		return b.readUTFBytes(len);
		#elseif php
		return b.getString(pos, len);
		#elseif cpp
		var result:String="";
		untyped __global__.__hxcpp_string_of_bytes(b,result,pos,len);
		return result;
		#elseif cs
		return cs.system.text.Encoding.UTF8.GetString(b, pos, len);
		#elseif java
		try
			return new String(b, pos, len, "UTF-8")
		catch (e:Dynamic) throw e;
		#elseif python
		return python.Syntax.pythonCode("b[{0}:{0}+{1}].decode('UTF-8','replace')", pos, len);
		#else
		var s = "";
		var b = b;
		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos+len;
		// utf8-decode and utf16-encode
		while( i < max ) {
			var c = b[i++];
			if( c < 0x80 ) {
				if( c == 0 ) break;
				s += fcc(c);
			} else if( c < 0xE0 )
				s += fcc( ((c & 0x3F) << 6) | (b[i++] & 0x7F) );
			else if( c < 0xF0 ) {
				var c2 = b[i++];
				s += fcc( ((c & 0x1F) << 12) | ((c2 & 0x7F) << 6) | (b[i++] & 0x7F) );
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				var u = ((c & 0x0F) << 18) | ((c2 & 0x7F) << 12) | ((c3 & 0x7F) << 6) | (b[i++] & 0x7F);
				// surrogate pair
				s += fcc( (u >> 10) + 0xD7C0 );
				s += fcc( (u & 0x3FF) | 0xDC00 );
			}
		}
		return s;
		#end
	}

	public inline static function fastGet( b : BytesData, pos : Int ) : Int {
		#if neko
		return untyped __dollar__sget(b,pos);
		#elseif flash
		return b[pos];
		#elseif php
		return b.get(pos);
		#elseif cpp
		return untyped b.unsafeGet(pos);
		#elseif java
		return untyped b[pos] & 0xFF;
		#else
		return b[pos];
		#end
	}

	#end

}
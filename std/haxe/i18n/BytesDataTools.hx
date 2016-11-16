package haxe.i18n;

import haxe.io.BytesData;
import haxe.io.Error;

#if cpp
using cpp.NativeArray;
#end

class BytesDataTools {

	#if (!(js))

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
		#elseif hl
		var b = new hl.types.Bytes(length);
		b.fill(0, length, 0);
		return new BytesData(b,length);
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
		#elseif hl
		return if ((pos:UInt) >= (getLength(b) : UInt)) 0 else b[pos];
		#else
		return b[pos];
		#end
	}

	public static function blit( b:BytesData, pos : Int, src : BytesData, srcpos : Int, len : Int ) : Void {
		#if !neko
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > getLength(b) || srcpos + len > getLength(src) ) throw Error.OutsideBounds;
		#end
		#if neko
		try untyped $sblit(b,pos,src,srcpos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif php
		b.blit(pos, src, srcpos, len);
		#elseif flash
		b.position = pos;
		if( len > 0 ) b.writeBytes(src,srcpos,len);
		#elseif java
		java.lang.System.arraycopy(src, srcpos, b, pos, len);
		#elseif cs
		cs.system.Array.Copy(src, srcpos, b, pos, len);
		#elseif python
		python.Syntax.pythonCode("b[{0}:{0}+{1}] = src[srcpos:srcpos+{1}]", pos, len);
		#elseif cpp
		b.blit(pos, src, srcpos, len);
		#elseif hl
		b.bytes.blit(pos, src.bytes, srcpos, len);
		#else
		var b1 = b;
		var b2 = src;
		if( b1 == b2 && pos > srcpos ) {
			var i = len;
			while( i > 0 ) {
				i--;
				b1[i + pos] = b2[i + srcpos];
			}
			return;
		}
		for( i in 0...len )
			b1[i+pos] = b2[i+srcpos];
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
		#elseif hl
		b[pos] = v;
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
		#elseif hl
		return b.length;
		#elseif lua
		return b.length;
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
		#elseif hl
		return new BytesData(b.bytes.sub(pos, len), len);
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
		#elseif lua
		var begin = cast(Math.min(pos, getLength(b)),Int);
		var end = cast(Math.min(pos+len,getLength(b)),Int);
		return [for (i in begin...end) String.fromCharCode(b[i])].join("");
		#elseif hl
		var b1 = new hl.types.Bytes(len + 1);
		b1.blit(0, b, pos, len);
		b1[len] = 0;
		return @:privateAccess String.fromUTF8(b1);
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
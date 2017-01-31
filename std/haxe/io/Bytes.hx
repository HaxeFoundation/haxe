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
package haxe.io;

#if cpp
using cpp.NativeArray;
#end

class Bytes {

	public var length(default,null) : Int;
	var b : BytesData;

	function new(length,b) {
		this.length = length;
		this.b = b;
		#if flash
		b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#end
	}

	public inline function get( pos : Int ) : Int {
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

	public inline function set( pos : Int, v : Int ) : Void {
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

	public function blit( pos : Int, src : Bytes, srcpos : Int, len : Int ) : Void {
		#if !neko
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length ) throw Error.OutsideBounds;
		#end
		#if neko
		try untyped $sblit(b,pos,src.b,srcpos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif php
		b.blit(pos, src.b, srcpos, len);
		#elseif flash
		b.position = pos;
		if( len > 0 ) b.writeBytes(src.b,srcpos,len);
		#elseif java
		java.lang.System.arraycopy(src.b, srcpos, b, pos, len);
		#elseif cs
		cs.system.Array.Copy(src.b, srcpos, b, pos, len);
		#elseif python
		python.Syntax.pythonCode("self.b[{0}:{0}+{1}] = src.b[srcpos:srcpos+{1}]", pos, len);
		#elseif cpp
		b.blit(pos, src.b, srcpos, len);
		#else
		var b1 = b;
		var b2 = src.b;
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

	public function fill( pos : Int, len : Int, value : Int ) {
		#if flash
		var v4 = value&0xFF;
		v4 |= v4<<8;
		v4 |= v4<<16;
		b.position = pos;
		for( i in 0...len>>2 )
			b.writeUnsignedInt(v4);
		pos += len&~3;
		for( i in 0...len&3 )
			set(pos++,value);
		#elseif cpp
		untyped __global__.__hxcpp_memory_memset(b,pos,len,value);
		#else
		for( i in 0...len )
			set(pos++, value);
		#end
	}

	public function sub( pos : Int, len : Int ) : Bytes {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > length ) throw Error.OutsideBounds;
		#end
		#if neko
		return try new Bytes(len,untyped __dollar__ssub(b,pos,len)) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash
		b.position = pos;
		var b2 = new flash.utils.ByteArray();
		b.readBytes(b2,0,len);
		return new Bytes(len,b2);
		#elseif php
		return new Bytes(len, b.sub(pos, len));
		#elseif java
		var newarr = new java.NativeArray(len);
		java.lang.System.arraycopy(b, pos, newarr, 0, len);
		return new Bytes(len, newarr);
		#elseif cs
		var newarr = new cs.NativeArray(len);
		cs.system.Array.Copy(b, pos, newarr, 0, len);
		return new Bytes(len, newarr);
		#elseif python
		return new Bytes(len, python.Syntax.arrayAccess(b, pos, pos+len) );
		#else
		return new Bytes(len,b.slice(pos,pos+len));
		#end
	}

	public function compare( other : Bytes ) : Int {
		#if neko
		return untyped __dollar__compare(b,other.b);
		#elseif flash
		var len = (length < other.length) ? length : other.length;
		var b1 = b;
		var b2 = other.b;
		b1.position = 0;
		b2.position = 0;
		b1.endian = flash.utils.Endian.BIG_ENDIAN;
		b2.endian = flash.utils.Endian.BIG_ENDIAN;
		for( i in 0...len>>2 )
			if( b1.readUnsignedInt() != b2.readUnsignedInt() ) {
				b1.position -= 4;
				b2.position -= 4;
				var d = b1.readUnsignedInt() - b2.readUnsignedInt();
				b1.endian = flash.utils.Endian.LITTLE_ENDIAN;
				b2.endian = flash.utils.Endian.LITTLE_ENDIAN;
				return d;
			}
		for( i in 0...len & 3 )
			if( b1.readUnsignedByte() != b2.readUnsignedByte() ) {
				b1.endian = flash.utils.Endian.LITTLE_ENDIAN;
				b2.endian = flash.utils.Endian.LITTLE_ENDIAN;
				return b1[b1.position-1] - b2[b2.position-1];
			}
		b1.endian = flash.utils.Endian.LITTLE_ENDIAN;
		b2.endian = flash.utils.Endian.LITTLE_ENDIAN;
		return length - other.length;
		#elseif php
		return b.compare(other.b);
		//#elseif cs
		//TODO: memcmp if unsafe flag is on
		#elseif cpp
		return b.memcmp(other.b);
		#else
		var b1 = b;
		var b2 = other.b;
		var len = (length < other.length) ? length : other.length;
		for( i in 0...len )
			if( b1[i] != b2[i] )
				return untyped b1[i] - untyped b2[i];
		return length - other.length;
		#end
	}


	/**
		Returns the IEEE double precision value at given position (in low endian encoding).
		Result is unspecified if reading outside of the bounds
	**/
	#if (neko_v21 || (cpp && !cppia) || flash) inline #end
	public function getDouble( pos : Int ) : Float {
		#if neko_v21
		return untyped $sgetd(b, pos, false);
		#elseif flash
		b.position = pos;
		return b.readDouble();
		#elseif cpp
		if( pos < 0 || pos + 8 > length ) throw Error.OutsideBounds;
		return untyped __global__.__hxcpp_memory_get_double(b,pos);
		#else
		return FPHelper.i64ToDouble(getInt32(pos),getInt32(pos+4));
		#end
	}

	/**
		Returns the IEEE single precision value at given position (in low endian encoding).
		Result is unspecified if reading outside of the bounds
	**/
	#if (neko_v21 || (cpp && !cppia) || flash) inline #end
	public function getFloat( pos : Int ) : Float {
		#if neko_v21
		return untyped $sgetf(b, pos, false);
		#elseif flash
		b.position = pos;
		return b.readFloat();
		#elseif cpp
		if( pos < 0 || pos + 4 > length ) throw Error.OutsideBounds;
		return untyped __global__.__hxcpp_memory_get_float(b,pos);
		#else
		var b = new haxe.io.BytesInput(this,pos,4);
		return b.readFloat();
		#end
	}

	/**
		Store the IEEE double precision value at given position in low endian encoding.
		Result is unspecified if writing outside of the bounds.
	**/
	#if (neko_v21 || flash) inline #end
	public function setDouble( pos : Int, v : Float ) : Void {
		#if neko_v21
		untyped $ssetd(b, pos, v, false);
		#elseif neko
		untyped $sblit(b, pos, FPHelper._double_bytes(v,false), 0, 8);
		#elseif flash
		b.position = pos;
		b.writeDouble(v);
		#elseif cpp
		if( pos < 0 || pos + 8 > length ) throw Error.OutsideBounds;
		untyped __global__.__hxcpp_memory_set_double(b,pos,v);
		#else
		var i = FPHelper.doubleToI64(v);
		setInt32(pos, i.low);
		setInt32(pos + 4, i.high);
		#end
	}

	/**
		Store the IEEE single precision value at given position in low endian encoding.
		Result is unspecified if writing outside of the bounds.
	**/
	#if (neko_v21 || flash) inline #end
	public function setFloat( pos : Int, v : Float ) : Void {
		#if neko_v21
		untyped $ssetf(b, pos, v, false);
		#elseif neko
		untyped $sblit(b, pos, FPHelper._float_bytes(v,false), 0, 4);
		#elseif flash
		b.position = pos;
		b.writeFloat(v);
		#elseif cpp
		if( pos < 0 || pos + 4 > length ) throw Error.OutsideBounds;
		untyped __global__.__hxcpp_memory_set_float(b,pos,v);
		#else
		setInt32(pos, FPHelper.floatToI32(v));
		#end
	}

	/**
		Returns the 16 bit unsigned integer at given position (in low endian encoding).
	**/
	public inline function getUInt16( pos : Int ) : Int {
		#if neko_v21
		return untyped $sget16(b, pos, false);
		#else
		return get(pos) | (get(pos + 1) << 8);
		#end
	}

	/**
		Store the 16 bit unsigned integer at given position (in low endian encoding).
	**/
	public inline function setUInt16( pos : Int, v : Int ) : Void {
		#if neko_v21
		untyped $sset16(b, pos, v, false);
		#else
		set(pos, v);
		set(pos + 1, v >> 8);
		#end
	}

	/**
		Returns the 32 bit integer at given position (in low endian encoding).
	**/
	public inline function getInt32( pos : Int ) : Int {
		#if neko_v21
		return untyped $sget32(b, pos, false);
		#elseif (php || python)
		var v = get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos+3) << 24);
		return if( v & 0x80000000 != 0 ) v | 0x80000000 else v;
		#elseif lua
		var v = get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos+3) << 24);
		return lua.Boot.clamp(if( v & 0x80000000 != 0 ) v | 0x80000000 else v);
		#else
		return get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos+3) << 24);
		#end
	}

	/**
		Returns the 64 bit integer at given position (in low endian encoding).
	**/
	public inline function getInt64( pos : Int ) : haxe.Int64 {
		return haxe.Int64.make(getInt32(pos+4),getInt32(pos));
	}

	/**
		Store the 32 bit integer at given position (in low endian encoding).
	**/
	public inline function setInt32( pos : Int, v : Int ) : Void {
		#if neko_v21
		untyped $sset32(b, pos, v, false);
		#else
		set(pos, v);
		set(pos + 1, v >> 8);
		set(pos + 2, v >> 16);
		set(pos + 3, v >>> 24);
		#end
	}

	/**
		Store the 64 bit integer at given position (in low endian encoding).
	**/
	public inline function setInt64( pos : Int, v : haxe.Int64 ) : Void {
		setInt32(pos, v.low);
		setInt32(pos + 4, v.high);
	}

	public function getString( pos : Int, len : Int ) : String {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > length ) throw Error.OutsideBounds;
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
		return python.Syntax.pythonCode("self.b[{0}:{0}+{1}].decode('UTF-8','replace')", pos, len);
		#elseif lua
		var begin = cast(Math.min(pos,b.length),Int);
		var end = cast(Math.min(pos+len,b.length),Int);
		return [for (i in begin...end) String.fromCharCode(b[i])].join("");
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

	@:deprecated("readString is deprecated, use getString instead")
	@:noCompletion
	public inline function readString(pos:Int, len:Int):String {
		return getString(pos, len);
	}

	public function toString() : String {
		#if neko
		return new String(untyped __dollar__ssub(b,0,length));
		#elseif flash
		b.position = 0;
		return b.readUTFBytes(length);
		#elseif php
		return b.toString();
		#elseif cs
		return cs.system.text.Encoding.UTF8.GetString(b, 0, length);
		#elseif java
		try
		{
			return new String(b, 0, length, "UTF-8");
		}
		catch (e:Dynamic) throw e;
		#else
		return getString(0,length);
		#end
	}

	public function toHex() : String {
		var s = new StringBuf();
		var chars = [];
		var str = "0123456789abcdef";
		for( i in 0...str.length )
			chars.push(str.charCodeAt(i));
		for( i in 0...length ) {
			var c = get(i);
			s.addChar(chars[c >> 4]);
			s.addChar(chars[c & 15]);
		}
		return s.toString();
	}

	public inline function getData() : BytesData {
		return b;
	}

	public static function alloc( length : Int ) : Bytes {
		#if neko
		return new Bytes(length,untyped __dollar__smake(length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.length = length;
		return new Bytes(length,b);
		#elseif php
		return new Bytes(length, BytesData.alloc(length));
		#elseif cpp
		var a = new BytesData();
		if (length>0) cpp.NativeArray.setSize(a, length);
		return new Bytes(length, a);
		#elseif cs
		return new Bytes(length, new cs.NativeArray(length));
		#elseif java
		return new Bytes(length, new java.NativeArray(length));
		#elseif python
		return new Bytes(length, new python.Bytearray(length));
		#else
		var a = new Array();
		for( i in 0...length )
			a.push(0);
		return new Bytes(length,a);
		#end
	}

	@:pure
	public static function ofString( s : String ) : Bytes {
		#if neko
		return new Bytes(s.length,untyped __dollar__ssub(s.__s,0,s.length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.writeUTFBytes(s);
		return new Bytes(b.length,b);
		#elseif php
		var x = BytesData.ofString(s);
		return new Bytes(x.length, x);
		#elseif cpp
		var a = new BytesData();
		untyped __global__.__hxcpp_bytes_of_string(a,s);
		return new Bytes(a.length, a);
		#elseif cs
		var b = cs.system.text.Encoding.UTF8.GetBytes(s);
		return new Bytes(b.Length, b);
		#elseif java
		try
		{
			var b:BytesData = untyped s.getBytes("UTF-8");
			return new Bytes(b.length, b);
		}
		catch (e:Dynamic) throw e;

		#elseif python
			var b:BytesData = new python.Bytearray(s, "UTF-8");
			return new Bytes(b.length, b);

		#elseif lua
			var bytes = [for (c in 0...s.length) StringTools.fastCodeAt(s,c)];
			return new Bytes(bytes.length, bytes);
		#else
		var a = new Array();
		// utf16-decode and utf8-encode
		var i = 0;
		while( i < s.length ) {
			var c : Int = StringTools.fastCodeAt(s,i++);
			// surrogate pair
			if( 0xD800 <= c && c <= 0xDBFF )
			       c = (c - 0xD7C0 << 10) | (StringTools.fastCodeAt(s,i++) & 0x3FF);
			if( c <= 0x7F )
				a.push(c);
			else if( c <= 0x7FF ) {
				a.push( 0xC0 | (c >> 6) );
				a.push( 0x80 | (c & 63) );
			} else if( c <= 0xFFFF ) {
				a.push( 0xE0 | (c >> 12) );
				a.push( 0x80 | ((c >> 6) & 63) );
				a.push( 0x80 | (c & 63) );
			} else {
				a.push( 0xF0 | (c >> 18) );
				a.push( 0x80 | ((c >> 12) & 63) );
				a.push( 0x80 | ((c >> 6) & 63) );
				a.push( 0x80 | (c & 63) );
			}
		}
		return new Bytes(a.length,a);
		#end
	}

	public static function ofData( b : BytesData ) {
		#if flash
		return new Bytes(b.length,b);
		#elseif neko
		return new Bytes(untyped __dollar__ssize(b),b);
		#elseif php
		return new Bytes(b.length, b);
		#elseif cs
		return new Bytes(b.Length,b);
		#else
		return new Bytes(b.length,b);
		#end
	}

	/**
		Read the most efficiently possible the n-th byte of the data.
		Behavior when reading outside of the available data is unspecified.
	**/
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

}

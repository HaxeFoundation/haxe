package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.io.Error;
import haxe.io.BytesData;
import haxe.io.Error;

#if cpp
using cpp.NativeArray;
#end

abstract ByteAccess(BytesData) {

	public var length(get, never):Int;

	inline function new (length:Int) {
		this = BytesDataTools.alloc(length);
	}

	public inline function get( pos : Int ) : Int {
		return BytesDataTools.get(this, pos);
	}

	public inline function getInt32( pos : Int ) : Int {
		return (get(pos) << 24) | (get(pos+1) << 16) | (get(pos+2) << 8) | get(pos+3);
	}

	public inline function getInt16( pos : Int ) : Int {
		var upper = get(pos) << 8;
		var lower =  get(pos+1);
		return upper | lower;
	}

	public inline function set( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, v);
	}

	public inline function setInt16( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, (v >> 8) & 0xFF );
		BytesDataTools.set(this, pos+1, v & 0xFF );
	}

	public inline function setInt32( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, (v >> 24) & 0xFF );
		BytesDataTools.set(this, pos+1, (v >> 16) & 0xFF );
		BytesDataTools.set(this, pos+2, (v >> 8) & 0xFF );
		BytesDataTools.set(this, pos+3, v & 0xFF );
	}

	inline function get_length ():Int {
		return BytesDataTools.getLength(this);
	}

	public inline function sub(i:Int, size:Int):ByteAccess {
		return fromBytesData(BytesDataTools.sub(this, i, size));
	}

	public inline function fastGet (pos:Int) {
		return BytesDataTools.fastGet(this, pos);
	}

	public inline function blit (pos : Int, src : ByteAccess, srcpos : Int, len : Int):Void {
		return BytesDataTools.blit(this, pos, src.asBytesData(), srcpos, len);
	}

	public inline function append (other : ByteAccess):ByteAccess {
		var ba = alloc(length + other.length);
		ba.blit(0, asByteAccess(), 0, length);
		ba.blit(length, other, 0, other.length);
		return ba;
	}

	@:op(a == b) function opEq (other: ByteAccess) {
		return asByteAccess().equal(other);
	}

	public static inline function alloc (length:Int) {
		return new ByteAccess(length);
	}

	public inline function copy ():ByteAccess {
		return fromBytesData(BytesDataTools.sub(this, 0, length));
	}

	public function equal (other:ByteAccess) {
		var a = fromBytesData(this);
		var b = other;
		
		if (a.length != b.length) return false;

		for (i in 0...a.length) {
			if (a.fastGet(i) != b.fastGet(i)) return false;
		}
		return true;
	}

	public function compare (other:ByteAccess) {
		var a = fromBytesData(this);
		var b = other;
		
		var min = a.length < b.length ? a.length : b.length;

		for (i in 0...min) {
			var b1 = a.fastGet(i);
			var b2 = b.fastGet(i);
			if (b1 < b2) return -1;
			if (b1 > b2) return 1;
		}
		if (a.length < b.length) return -1;
		if (a.length > b.length) return 1;
		return 0;
	}

	public function toString ():String {
		var a = fromBytesData(this);
		var res = [];
		for (i in 0...a.length) {
			
			res.push(a.fastGet(i));
		}
		return res.join(",");
	}

	public inline function getString( pos : Int, len : Int ) : String {
		return BytesDataTools.getString(this, pos, len);
	}

	static inline function fromBytesData (b:BytesData):ByteAccess {
		return cast b;
	}

	public inline function asBytesData ():BytesData {
		return this;
	}

	inline function asByteAccess ():ByteAccess {
		return cast this;
	}

	public static inline function ofData (data:BytesData):ByteAccess {
		return fromBytesData(data);
	}

	public static inline function fromBytes (b:Bytes):ByteAccess {
		return fromBytesData(b.getData());
	}

	public inline function toBytes ():Bytes {
		return Bytes.ofData(this);
	}
}

private class BytesDataTools {

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
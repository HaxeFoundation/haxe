package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.io.Error;

#if !nodejs
import js.html.compat.Uint8Array;
#end


import haxe.io.BytesData;
import haxe.io.Error;

#if !nodejs
import js.html.compat.Uint8Array;
#end
import js.html.Uint8Array;

private class Uint8ArrayTools {

	#if (js)
	public static inline function get( b:Uint8Array, pos : Int ) : Int {
		return b[pos];
	}

	public static inline function set( b:Uint8Array, pos : Int, v : Int ) : Void {
		b[pos] = v & 0xFF;
	}
	public static function getString( b:Uint8Array, pos : Int, len : Int ) : String {
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		var s = "";

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
	}
	public static inline function alloc (length:Int) {
		var data = new BytesData(length);
		return Uint8ArrayTools.wrapData(data);
	}
	public static function sub(b:Uint8Array, pos:Int, len:Int):Uint8Array {
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		return wrapData(b.buffer.slice(pos+b.byteOffset,pos+b.byteOffset+len));
	}


	public static inline function fastGet (b:Uint8Array, pos:Int):Int {
		return (b:Dynamic)[pos];
	}

	public static inline function getData(b:Uint8Array):BytesData {
		return (b:Dynamic).bufferValue;
	}

	public static function getLength (b:Uint8Array):Int {
		return getData(b).byteLength;
	}
	public static inline function wrapData (data:BytesData):Uint8Array {
		var a = new js.html.Uint8Array(data);
		(a:Dynamic).bufferValue = data; // some impl does not return the same instance in .buffer
		(data:Dynamic).bytes = a;
		return a;
	}
	public static function blit( b:Uint8Array, pos : Int, src : Uint8Array, srcpos : Int, len : Int ) : Void {

		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > getLength(b) || srcpos + len > getLength(src) ) throw Error.OutsideBounds;

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
	}
	#end

}

abstract ByteAccess(Uint8Array) {

	public var length(get, never):Int;

	inline function new (length:Int) {
		this = Uint8ArrayTools.alloc(length);
	}

	public inline function get( pos : Int ) : Int {
		return Uint8ArrayTools.get(this, pos);
	}

	public inline function getInt32( pos : Int ) : Int {
		return (get(pos) << 24) | (get(pos+1) << 16) | (get(pos+2) << 8) | get(pos+3);
	}
	public inline function getInt16( pos : Int ) : Int {
		return (get(pos) << 8) | get(pos+1);
	}

	public inline function set( pos : Int, v : Int ) : Void {
		Uint8ArrayTools.set(this, pos, v);
	}

	public inline function setInt32( pos : Int, v : Int ) : Void {
		Uint8ArrayTools.set(this, pos, (v >> 24) & 0xFF );
		Uint8ArrayTools.set(this, pos+1, (v >> 16) & 0xFF );
		Uint8ArrayTools.set(this, pos+2, (v >> 8) & 0xFF );
		Uint8ArrayTools.set(this, pos+3, v & 0xFF );
	}

	inline function getData():BytesData {
		return Uint8ArrayTools.getData(this);
	}

	inline function get_length ():Int {
		return Uint8ArrayTools.getLength(this);
	}

	public inline function sub(pos:Int, len:Int):ByteAccess {
		return fromUint8Array(Uint8ArrayTools.sub(this, pos, len));
	}

	public function toString ():String {
		var res = [];
		for (i in 0...length) {
			
			res.push(fastGet(i));
		}
		return res.join(",");
	}

	public inline function fastGet (pos:Int):Int {
		return Uint8ArrayTools.fastGet(this, pos);
	}

	public static inline function alloc (length:Int) {
		return new ByteAccess(length);
	}

	public inline function copy ():ByteAccess {
		return sub(0, length);
	}

	public function equal (other:ByteAccess) {
		var a = fromUint8Array(this);
		var b = other;
		if (a.length != b.length) return false;

		for (i in 0...a.length) {
			if (a.fastGet(i) != b.fastGet(i)) return false;
		}
		return true;
	}
	
	public function compare (other:ByteAccess) {
		var a = fromUint8Array(this);
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

	public inline function getString( pos : Int, len : Int ) : String {
		return Uint8ArrayTools.getString(this, pos, len);
	}

	public inline function blit (pos : Int, src : ByteAccess, srcpos : Int, len : Int):Void {
		return Uint8ArrayTools.blit(this, pos, src.asUint8Array(), srcpos, len);
	}

	public inline function append (other : ByteAccess):ByteAccess {

		var c = Uint8ArrayTools.alloc(this.length + other.length);
		c.set(this);
		c.set(other.asUint8Array(), this.length);
		return fromUint8Array(c);
	}

	static inline function fromUint8Array (b:Uint8Array):ByteAccess {
		return cast b;
	}

	inline function asUint8Array ():Uint8Array {
		return this;
	}

	public static inline function fromBytes (b:Bytes):ByteAccess {
		return fromUint8Array(Uint8ArrayTools.wrapData(b.sub(0, b.length).getData()));
	}

	public static inline function ofData (data:BytesData):ByteAccess {
		return fromUint8Array(Uint8ArrayTools.wrapData(data));
	}

	public inline function toBytes ():Bytes {
		return Bytes.ofData(sub(0, length).getData());
	}
}
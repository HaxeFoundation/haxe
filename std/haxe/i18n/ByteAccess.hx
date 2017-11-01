package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.io.Error;

import haxe.i18n.BytesDataTools;

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
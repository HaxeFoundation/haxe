package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.io.Error;

#if js

#if !nodejs
import js.html.compat.Uint8Array;
#end
import haxe.i18n.Uint8ArrayTools;
import js.html.Uint8Array;

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


#else

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
		//if (upper == null) throw "upper is null";
		//if (lower == null) throw "lower is null";
		return upper | lower;
	}

	public inline function set( pos : Int, v : Int ) : Void {
		return BytesDataTools.set(this, pos, v);
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

#end

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


	static inline function fromUint8Array (b:Uint8Array):ByteAccess {
		return cast b;
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

	public inline function getString( pos : Int, len : Int ) : String {
		return BytesDataTools.getString(this, pos, len);
	}

	static inline function fromBytesData (b:BytesData):ByteAccess {
		return cast b;
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

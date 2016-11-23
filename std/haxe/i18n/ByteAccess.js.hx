package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.io.Error;


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


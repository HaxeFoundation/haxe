package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;

// TODO write a faster ByteAccessBuffer without using BytesBuffer or by
// adding addByteAccess and getByteAccess to BytesBuffer

@:forward(addByte, length) abstract ByteAccessBuffer(BytesBuffer) {

	public inline function new () {
		this = new BytesBuffer();
	}



	inline function asBytesBuffer ():BytesBuffer {
		return this;
	}

	public inline function add (b:ByteAccess) {
		this.add(b.toBytes());
	}

	public inline function addInt16BigEndian (i:Int) {
		this.addByte((i >> 8) & 0xFF);
		this.addByte(i & 0xFF);
	}

	public inline function reset ():Void {
		BytesBufferTools.reset(this);
	}

	public inline function addBuffer (buf:ByteAccessBuffer) {
		add(buf.getByteAccess());
	}

	public inline function getByteAccess ():ByteAccess {
		return ByteAccess.fromBytes(this.getBytes());
	}

}

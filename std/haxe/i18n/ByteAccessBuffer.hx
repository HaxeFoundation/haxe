package haxe.i18n;

import haxe.io.Bytes;

import haxe.io.BytesBuffer;

@:access(haxe.io.BytesBuffer)
class BytesBufferTools {

	public static function reset(buffer:BytesBuffer) {
		#if neko
		buffer.b = untyped StringBuf.__make();
		#elseif flash
		buffer.b = new flash.utils.ByteArray();
		buffer.b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#elseif php
		buffer.b = "";
		#elseif cpp
		buffer.b = new haxe.io.BytesData();
		#elseif cs
		buffer.b = new cs.system.io.MemoryStream();
		#elseif java
		buffer.b = new java.io.ByteArrayOutputStream();
		#elseif hl
		buffer.b = new hl.Bytes(buffer.size);
		buffer.pos = 0;
		#else
		buffer.b = new Array();
		#end
	}
}

// TODO write a faster ByteAccessBuffer without using BytesBuffer or by
// adding addByteAccess and getByteAccess to BytesBuffer

@:forward(addByte, length) abstract ByteAccessBuffer(BytesBuffer) {

	public inline function new () {
		this = new BytesBuffer();
	}

	public inline function add (b:ByteAccess) {
		this.add(b.toBytes());
	}

	public inline function addInt16BigEndian (i:Int) {
		this.addByte((i >> 8) & 0xFF);
		this.addByte(i & 0xFF);
	}

	public inline function addInt32BigEndian (i:Int) {
		this.addByte((i >> 24) & 0xFF);
		this.addByte((i >> 16) & 0xFF);
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
		var b = this.getBytes();
		return ByteAccess.fromBytes(b);
	}

}

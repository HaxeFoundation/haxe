package haxe.i18n;

import haxe.io.BytesBuffer;


@:access(haxe.io.BytesBuffer)
class BytesBufferTools {

	/*
	public static inline function addBytesData( buffer:BytesBuffer, src : Bytes ) {
		#if neko
		untyped StringBuf.__add(b,src.getData());
		#elseif flash
		b.writeBytes(src.getData());
		#elseif php
		b += src.getData().toString();
		#elseif cs
		b.Write(src.getData(), 0, src.length);
		#elseif java
		b.write(src.getData(), 0, src.length);
		#elseif js
		var b1 = b;
		var b2 = @:privateAccess src.b;
		for( i in 0...src.length )
			b.push(b2[i]);
		#else
		var b1 = b;
		var b2 = src.getData();
		for( i in 0...src.length )
			b.push(b2[i]);
		#end
	}

	public function getByteAccess(buffer:BytesBuffer) : Bytes untyped {
		#if neko
		var str = StringBuf.__to_string(b);
		var bytes = new Bytes(__dollar__ssize(str),str);
		#elseif flash
		var bytes = new Bytes(b.length,b);
		b.position = 0;
		#elseif php
		var bytes = new Bytes(b.length, BytesData.ofString(b));
		#elseif cs
		var buf = b.GetBuffer();
		var bytes = new Bytes(cast b.Length, buf);
		#elseif java
		var buf = b.toByteArray();
		var bytes = new Bytes(buf.length, buf);
		#elseif python
		var buf = new python.Bytearray(b);
		var bytes = new Bytes(buf.length, buf);
		#elseif js
		var bytes = new Bytes(new js.html.Uint8Array(b).buffer);
		#else
		var bytes = new Bytes(b.length,b);
		#end
		b = null;
		return bytes;
	}
	*/
	public static function reset(buffer:BytesBuffer) {
		#if neko
		buffer.b = untyped StringBuf.__make();
		#elseif flash
		buffer.b = new flash.utils.ByteArray();
		buffer.b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#elseif php
		buffer.b = "";
		#elseif cpp
		buffer.b = new BytesData();
		#elseif cs
		buffer.b = new cs.system.io.MemoryStream();
		#elseif java
		buffer.b = new java.io.ByteArrayOutputStream();
		#else
		buffer.b = new Array();
		#end
	}


}
/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.i18n;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;

@:access(haxe.io.BytesBuffer)
private class BytesBufferTools {

	public static inline function reset(buffer:BytesBuffer):BytesBuffer {
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
		#elseif eval
		buffer = new BytesBuffer();
		#else
		buffer.b = new Array();
		#end
		return buffer;
	}

	public static inline function addBytesData( buffer:BytesBuffer, src : haxe.io.BytesData ) {
		#if neko
		untyped StringBuf.__add(buffer.b,src);
		#elseif flash
		buffer.b.writeBytes(src);
		#elseif php
		buffer.b += src.toString();
		#elseif cs
		buffer.b.Write(src, 0, src.length);
		#elseif java
		buffer.b.write(src, 0, src.length);
		#elseif eval
		buffer.addBytes(src, 0, src.length);
		#elseif hl
		@:privateAccess buffer.__add(src, 0, src.length);

		#else
		var b1 = buffer.b;
		var b2 = src;
		for( i in 0...src.length )
			buffer.b.push(b2[i]);
		#end
	}
}

@:access(haxe.i18n.ByteAccess)
@:forward(addByte, length) abstract ByteAccessBuffer(BytesBuffer) {

	public inline function new () {
		this = new BytesBuffer();
	}

	public inline function add (b:ByteAccess) {
		BytesBufferTools.addBytesData(this, b.getData());
	}

	public inline function addInt16LE (i:Int) {
		this.addByte(i & 0xFF);
		this.addByte((i >> 8) & 0xFF);
	}

	public inline function addInt32LE (i:Int) {
		this.addByte(i & 0xFF);
		this.addByte((i >> 8) & 0xFF);
		this.addByte((i >> 16) & 0xFF);
		this.addByte((i >> 24) & 0xFF);
	}

	public inline function addBuffer (buf:ByteAccessBuffer) {
		#if (python)
		for (e in (@:privateAccess (buf.impl()).b)) {
			this.addByte(e);
		}
		#else
		add(buf.getByteAccess());
		#end
	}

	public inline function reset ():ByteAccessBuffer {
		return fromImpl(BytesBufferTools.reset(this));
	}

	public inline function getByteAccess ():ByteAccess {
		var b = this.getBytes();
		return ByteAccess.fromBytes(b);
	}

	/* internal */

	static inline function fromImpl (buf:BytesBuffer):ByteAccessBuffer {
		return cast buf;
	}

	inline function impl ():BytesBuffer {
		return this;
	}

}

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

		@:privateAccess {
			buffer.pos = 0;
			buffer.size = 0;
			buffer.view = null;
			buffer.u8 = null;
			buffer.buffer = null;
		}
		return buffer;
	}

	public static inline function addBytesData( buffer:BytesBuffer, src : haxe.io.BytesData ) {
		@:privateAccess {
			if( buffer.pos + src.byteLength > buffer.size ) buffer.grow(src.byteLength);
			if( buffer.size == 0 ) return;
			var bytes = ((src:Dynamic).bytes:js.html.Uint8Array);
			var sub = new js.html.Uint8Array(bytes.buffer, bytes.byteOffset, src.byteLength);
			buffer.u8.set(sub, buffer.pos);
			buffer.pos += src.byteLength;
		}

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
		var buf = buf.impl();
		@:privateAccess for (i in 0...buf.length) {
			this.addByte( buf.view.getInt8(i));
		}
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

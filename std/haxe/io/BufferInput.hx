/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.io;

class BufferInput extends haxe.io.Input {

	public var i : haxe.io.Input;
	public var buf : haxe.io.Bytes;
	public var available : Int;
	public var pos : Int;

	public function new(i, buf, ?pos = 0, ?available = 0) {
		this.i = i;
		this.buf = buf;
		this.pos = pos;
		this.available = available;
	}

	public function refill() {
		if( pos > 0 ) {
			buf.blit(0, buf, pos, available);
			pos = 0;
		}
		available += i.readBytes(buf, available, buf.length - available);
	}

	override function readByte() {
		if( available == 0 ) refill();
		var c = buf.get(pos);
		pos++;
		available--;
		return c;
	}

	override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) {
		if( available == 0 ) refill();
		var size = if( len > available ) available else len;
		buf.blit(pos, this.buf, this.pos, size);
		this.pos += size;
		this.available -= size;
		return size;
	}

}
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

class BytesOutput extends Output {

	var b : BytesBuffer;

	/** The length of the stream in bytes. **/
	public var length(get,never) : Int;

	public function new() {
		b = new BytesBuffer();
	}

	override function writeByte(c) {
		b.addByte(c);
	}

	override function writeBytes( buf : Bytes, pos, len ) : Int {
		b.addBytes(buf,pos,len);
		return len;
	}

	/**
		Returns the `Bytes` of this output.

		This function should not be called more than once on a given
		`BytesOutput` instance.
	**/
	public function getBytes() : Bytes {
		return b.getBytes();
	}

	inline function get_length() : Int {
		return b.length;
	}
}

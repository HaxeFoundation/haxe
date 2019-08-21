/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package haxe.crypto;

/**
	Calculates the Crc32 of the given Bytes.
 */
class Crc32 {
	var crc:Int;

	public inline function new() {
		crc = 0xFFFFFFFF;
	}

	public inline function byte(b:Int) {
		var tmp = (crc ^ b) & 0xFF;
		for (j in 0...8)
			tmp = (tmp >>> 1) ^ (-(tmp & 1) & 0xEDB88320);
		crc = (crc >>> 8) ^ tmp;
	}

	public inline function update(b:haxe.io.Bytes, pos, len) {
		var b = b.getData();
		for (i in pos...pos + len) {
			var tmp = (crc ^ haxe.io.Bytes.fastGet(b, i)) & 0xFF;
			for (j in 0...8)
				tmp = (tmp >>> 1) ^ (-(tmp & 1) & 0xEDB88320);
			crc = (crc >>> 8) ^ tmp;
		}
	}

	public inline function get() {
		return crc ^ 0xFFFFFFFF;
	}

	/**
		Calculates the CRC32 of the given data bytes
	**/
	public static function make(data:haxe.io.Bytes):Int {
		var c = new Crc32();
		c.update(data, 0, data.length);
		return c.get();
	}
}

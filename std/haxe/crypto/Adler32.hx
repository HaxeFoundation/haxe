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
	Calculates the Adler32 of the given Bytes.
 */
class Adler32 {
	var a1:Int;
	var a2:Int;

	public function new() {
		a1 = 1;
		a2 = 0;
	}

	public function get() {
		return (a2 << 16) | a1;
	}

	public function update(b:haxe.io.Bytes, pos, len) {
		var a1 = a1, a2 = a2;
		for (p in pos...pos + len) {
			var c = b.get(p);
			a1 = (a1 + c) % 65521;
			a2 = (a2 + a1) % 65521;
		}
		this.a1 = a1;
		this.a2 = a2;
	}

	public function equals(a:Adler32) {
		return a.a1 == a1 && a.a2 == a2;
	}

	public function toString() {
		return StringTools.hex(a2, 8) + StringTools.hex(a1, 8);
	}

	public static function read(i:haxe.io.Input) {
		var a = new Adler32();
		var a2a = i.readByte();
		var a2b = i.readByte();
		var a1a = i.readByte();
		var a1b = i.readByte();
		a.a1 = (a1a << 8) | a1b;
		a.a2 = (a2a << 8) | a2b;
		return a;
	}

	public static function make(b:haxe.io.Bytes) {
		var a = new Adler32();
		a.update(b, 0, b.length);
		return a.get();
	}
}

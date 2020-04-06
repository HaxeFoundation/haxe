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
	Allows one to encode/decode String and bytes using a power of two base dictionary.
**/
class BaseCode {
	var base:haxe.io.Bytes;
	var nbits:Int;
	var tbl:Array<Int>;

	public function new(base:haxe.io.Bytes) {
		var len = base.length;
		var nbits = 1;
		while (len > 1 << nbits)
			nbits++;
		if (nbits > 8 || len != 1 << nbits)
			throw "BaseCode : base length must be a power of two.";
		this.base = base;
		this.nbits = nbits;
	}

	public function encodeBytes(b:haxe.io.Bytes):haxe.io.Bytes {
		#if (neko && !interp)
		return haxe.io.Bytes.ofData(base_encode(b.getData(), base.getData()));
		#else
		var nbits = this.nbits;
		var base = this.base;
		var size = Std.int(b.length * 8 / nbits);
		var out = haxe.io.Bytes.alloc(size + (((b.length * 8) % nbits == 0) ? 0 : 1));
		var buf = 0;
		var curbits = 0;
		var mask = (1 << nbits) - 1;
		var pin = 0;
		var pout = 0;
		while (pout < size) {
			while (curbits < nbits) {
				curbits += 8;
				buf <<= 8;
				buf |= b.get(pin++);
			}
			curbits -= nbits;
			out.set(pout++, base.get((buf >> curbits) & mask));
		}
		if (curbits > 0)
			out.set(pout++, base.get((buf << (nbits - curbits)) & mask));
		return out;
		#end
	}

	function initTable() {
		var tbl = new Array();
		for (i in 0...256)
			tbl[i] = -1;
		for (i in 0...base.length)
			tbl[base.get(i)] = i;
		this.tbl = tbl;
	}

	public function decodeBytes(b:haxe.io.Bytes):haxe.io.Bytes {
		#if (neko && !interp)
		return haxe.io.Bytes.ofData(base_decode(b.getData(), base.getData()));
		#else
		var nbits = this.nbits;
		var base = this.base;
		if (this.tbl == null)
			initTable();
		var tbl = this.tbl;
		var size = (b.length * nbits) >> 3;
		var out = haxe.io.Bytes.alloc(size);
		var buf = 0;
		var curbits = 0;
		var pin = 0;
		var pout = 0;
		while (pout < size) {
			while (curbits < 8) {
				curbits += nbits;
				buf <<= nbits;
				var i = tbl[b.get(pin++)];
				if (i == -1)
					throw "BaseCode : invalid encoded char";
				buf |= i;
			}
			curbits -= 8;
			out.set(pout++, (buf >> curbits) & 0xFF);
		}
		return out;
		#end
	}

	public function encodeString(s:String) {
		#if (neko && !interp)
		return neko.NativeString.toString(base_encode(neko.NativeString.ofString(s), base.getData()));
		#else
		return encodeBytes(haxe.io.Bytes.ofString(s)).toString();
		#end
	}

	public function decodeString(s:String) {
		#if (neko && !interp)
		return neko.NativeString.toString(base_decode(neko.NativeString.ofString(s), base.getData()));
		#else
		return decodeBytes(haxe.io.Bytes.ofString(s)).toString();
		#end
	}

	public static function encode(s:String, base:String) {
		var b = new BaseCode(haxe.io.Bytes.ofString(base));
		return b.encodeString(s);
	}

	public static function decode(s:String, base:String) {
		var b = new BaseCode(haxe.io.Bytes.ofString(base));
		return b.decodeString(s);
	}

	#if neko
	private static var base_encode = neko.Lib.load("std", "base_encode", 2);
	private static var base_decode = neko.Lib.load("std", "base_decode", 2);
	#end
}

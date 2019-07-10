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
	Creates a Sha1 of a String.
**/
class Sha1 {
	public static function encode(s:String):String {
		var sh = new Sha1();
		var h = sh.doEncode(str2blks(s));
		return sh.hex(h);
	}

	public static function make(b:haxe.io.Bytes):haxe.io.Bytes {
		var h = new Sha1().doEncode(bytes2blks(b));
		var out = haxe.io.Bytes.alloc(20);
		var p = 0;
		for (i in 0...5) {
			out.set(p++, h[i] >>> 24);
			out.set(p++, (h[i] >> 16) & 0xFF);
			out.set(p++, (h[i] >> 8) & 0xFF);
			out.set(p++, h[i] & 0xFF);
		}
		return out;
	}

	function new() {}

	function doEncode(x:Array<Int>):Array<Int> {
		var w = new Array<Int>();

		var a = 0x67452301;
		var b = 0xEFCDAB89;
		var c = 0x98BADCFE;
		var d = 0x10325476;
		var e = 0xC3D2E1F0;

		var i = 0;
		while (i < x.length) {
			var olda = a;
			var oldb = b;
			var oldc = c;
			var oldd = d;
			var olde = e;

			var j = 0;
			while (j < 80) {
				if (j < 16)
					w[j] = x[i + j];
				else
					w[j] = rol(w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16], 1);
				var t = rol(a, 5) + ft(j, b, c, d) + e + w[j] + kt(j);
				e = d;
				d = c;
				c = rol(b, 30);
				b = a;
				a = t;
				j++;
			}
			a += olda;
			b += oldb;
			c += oldc;
			d += oldd;
			e += olde;
			i += 16;
		}
		return [a, b, c, d, e];
	}

	/**
		Convert a string to a sequence of 16-word blocks, stored as an array.
		Append padding bits and the length, as described in the SHA1 standard.
	**/
	static function str2blks(s:String):Array<Int> {
		#if target.unicode
		var s = haxe.io.Bytes.ofString(s);
		#end
		var nblk = ((s.length + 8) >> 6) + 1;
		var blks = new Array<Int>();

		for (i in 0...nblk * 16)
			blks[i] = 0;
		for (i in 0...s.length) {
			var p = i >> 2;
			blks[p] |= #if target.unicode s.get(i) #else StringTools.fastCodeAt(s, i) #end << (24 - ((i & 3) << 3));
		}
		var i = s.length;
		var p = i >> 2;
		blks[p] |= 0x80 << (24 - ((i & 3) << 3));
		blks[nblk * 16 - 1] = s.length * 8;
		return blks;
	}

	static function bytes2blks(b:haxe.io.Bytes):Array<Int> {
		var nblk = ((b.length + 8) >> 6) + 1;
		var blks = new Array<Int>();

		for (i in 0...nblk * 16)
			blks[i] = 0;
		for (i in 0...b.length) {
			var p = i >> 2;
			blks[p] |= b.get(i) << (24 - ((i & 3) << 3));
		}
		var i = b.length;
		var p = i >> 2;
		blks[p] |= 0x80 << (24 - ((i & 3) << 3));
		blks[nblk * 16 - 1] = b.length * 8;
		return blks;
	}

	/**
		Bitwise rotate a 32-bit number to the left
	**/
	inline function rol(num:Int, cnt:Int):Int {
		return (num << cnt) | (num >>> (32 - cnt));
	}

	/**
		Perform the appropriate triplet combination function for the current iteration
	**/
	function ft(t:Int, b:Int, c:Int, d:Int):Int {
		if (t < 20)
			return (b & c) | ((~b) & d);
		if (t < 40)
			return b ^ c ^ d;
		if (t < 60)
			return (b & c) | (b & d) | (c & d);
		return b ^ c ^ d;
	}

	/**
		Determine the appropriate additive constant for the current iteration
	**/
	function kt(t:Int):Int {
		if (t < 20)
			return 0x5A827999;
		if (t < 40)
			return 0x6ED9EBA1;
		if (t < 60)
			return 0x8F1BBCDC;
		return 0xCA62C1D6;
	}

	function hex(a:Array<Int>) {
		var str = "";
		for (num in a) {
			str += StringTools.hex(num, 8);
		}
		return str.toLowerCase();
	}
}

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
	Creates a MD5 of a String.
**/
class Md5 {
	public static function encode(s:String):String {
		var m = new Md5();
		var h = m.doEncode(str2blks(s));
		return m.hex(h);
	}

	public static function make(b:haxe.io.Bytes):haxe.io.Bytes {
		var h = new Md5().doEncode(bytes2blks(b));
		var out = haxe.io.Bytes.alloc(16);
		var p = 0;
		for (i in 0...4) {
			out.set(p++, h[i] & 0xFF);
			out.set(p++, (h[i] >> 8) & 0xFF);
			out.set(p++, (h[i] >> 16) & 0xFF);
			out.set(p++, h[i] >>> 24);
		}
		return out;
	}

	/*
	 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
	 * Digest Algorithm, as defined in RFC 1321.
	 * Copyright (C) Paul Johnston 1999 - 2000.
	 * Updated by Greg Holt 2000 - 2001.
	 * See http://pajhome.org.uk/site/legal.html for details.
	 */
	function new() {}

	function bitOR(a, b) {
		var lsb = (a & 0x1) | (b & 0x1);
		var msb31 = (a >>> 1) | (b >>> 1);
		return (msb31 << 1) | lsb;
	}

	function bitXOR(a, b) {
		var lsb = (a & 0x1) ^ (b & 0x1);
		var msb31 = (a >>> 1) ^ (b >>> 1);
		return (msb31 << 1) | lsb;
	}

	function bitAND(a, b) {
		var lsb = (a & 0x1) & (b & 0x1);
		var msb31 = (a >>> 1) & (b >>> 1);
		return (msb31 << 1) | lsb;
	}

	function addme(x, y) {
		var lsw = (x & 0xFFFF) + (y & 0xFFFF);
		var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
		return (msw << 16) | (lsw & 0xFFFF);
	}

	function hex(a:Array<Int>) {
		var str = "";
		var hex_chr = "0123456789abcdef";
		for (num in a)
			for (j in 0...4)
				str += hex_chr.charAt((num >> (j * 8 + 4)) & 0x0F) + hex_chr.charAt((num >> (j * 8)) & 0x0F);
		return str;
	}

	static function bytes2blks(b:haxe.io.Bytes) {
		var nblk = ((b.length + 8) >> 6) + 1;
		var blks = new Array();

		// preallocate size
		var blksSize = nblk * 16;
		#if (neko || cs || cpp || java || hl)
		blks[blksSize - 1] = 0;
		#end

		#if !(cpp || cs || hl) // C++ and C# will already initialize them with zeroes.
		for (i in 0...blksSize)
			blks[i] = 0;
		#end

		var i = 0;
		while (i < b.length) {
			blks[i >> 2] |= b.get(i) << ((((b.length << 3) + i) & 3) << 3);
			i++;
		}
		blks[i >> 2] |= 0x80 << (((b.length * 8 + i) % 4) * 8);
		var l = b.length * 8;
		var k = nblk * 16 - 2;
		blks[k] = (l & 0xFF);
		blks[k] |= ((l >>> 8) & 0xFF) << 8;
		blks[k] |= ((l >>> 16) & 0xFF) << 16;
		blks[k] |= ((l >>> 24) & 0xFF) << 24;
		return blks;
	}

	static function str2blks(str:String) {
		#if target.unicode
		var str = haxe.io.Bytes.ofString(str);
		#end
		var nblk = ((str.length + 8) >> 6) + 1;
		var blks = new Array();

		// preallocate size
		var blksSize = nblk * 16;
		#if (neko || eval || cs || cpp || java || hl)
		blks[blksSize - 1] = 0;
		#end

		#if !(cpp || cs || hl) // C++ and C# will already initialize them with zeroes.
		for (i in 0...blksSize)
			blks[i] = 0;
		#end

		var i = 0;
		var max = str.length;
		var l = max * 8;
		while (i < max) {
			blks[i >> 2] |= #if target.unicode str.get(i) #else StringTools.fastCodeAt(str, i) #end << (((l + i) % 4) * 8);
			i++;
		}
		blks[i >> 2] |= 0x80 << (((l + i) % 4) * 8);
		var k = nblk * 16 - 2;
		blks[k] = (l & 0xFF);
		blks[k] |= ((l >>> 8) & 0xFF) << 8;
		blks[k] |= ((l >>> 16) & 0xFF) << 16;
		blks[k] |= ((l >>> 24) & 0xFF) << 24;
		return blks;
	}

	function rol(num, cnt) {
		return (num << cnt) | (num >>> (32 - cnt));
	}

	function cmn(q, a, b, x, s, t) {
		return addme(rol((addme(addme(a, q), addme(x, t))), s), b);
	}

	function ff(a, b, c, d, x, s, t) {
		return cmn(bitOR(bitAND(b, c), bitAND((~b), d)), a, b, x, s, t);
	}

	function gg(a, b, c, d, x, s, t) {
		return cmn(bitOR(bitAND(b, d), bitAND(c, (~d))), a, b, x, s, t);
	}

	function hh(a, b, c, d, x, s, t) {
		return cmn(bitXOR(bitXOR(b, c), d), a, b, x, s, t);
	}

	function ii(a, b, c, d, x, s, t) {
		return cmn(bitXOR(c, bitOR(b, (~d))), a, b, x, s, t);
	}

	function doEncode(x:Array<Int>):Array<Int> {
		var a = 1732584193;
		var b = -271733879;
		var c = -1732584194;
		var d = 271733878;

		var step;

		var i = 0;
		while (i < x.length) {
			var olda = a;
			var oldb = b;
			var oldc = c;
			var oldd = d;

			step = 0;
			a = ff(a, b, c, d, x[i + 0], 7, -680876936);
			d = ff(d, a, b, c, x[i + 1], 12, -389564586);
			c = ff(c, d, a, b, x[i + 2], 17, 606105819);
			b = ff(b, c, d, a, x[i + 3], 22, -1044525330);
			a = ff(a, b, c, d, x[i + 4], 7, -176418897);
			d = ff(d, a, b, c, x[i + 5], 12, 1200080426);
			c = ff(c, d, a, b, x[i + 6], 17, -1473231341);
			b = ff(b, c, d, a, x[i + 7], 22, -45705983);
			a = ff(a, b, c, d, x[i + 8], 7, 1770035416);
			d = ff(d, a, b, c, x[i + 9], 12, -1958414417);
			c = ff(c, d, a, b, x[i + 10], 17, -42063);
			b = ff(b, c, d, a, x[i + 11], 22, -1990404162);
			a = ff(a, b, c, d, x[i + 12], 7, 1804603682);
			d = ff(d, a, b, c, x[i + 13], 12, -40341101);
			c = ff(c, d, a, b, x[i + 14], 17, -1502002290);
			b = ff(b, c, d, a, x[i + 15], 22, 1236535329);
			a = gg(a, b, c, d, x[i + 1], 5, -165796510);
			d = gg(d, a, b, c, x[i + 6], 9, -1069501632);
			c = gg(c, d, a, b, x[i + 11], 14, 643717713);
			b = gg(b, c, d, a, x[i + 0], 20, -373897302);
			a = gg(a, b, c, d, x[i + 5], 5, -701558691);
			d = gg(d, a, b, c, x[i + 10], 9, 38016083);
			c = gg(c, d, a, b, x[i + 15], 14, -660478335);
			b = gg(b, c, d, a, x[i + 4], 20, -405537848);
			a = gg(a, b, c, d, x[i + 9], 5, 568446438);
			d = gg(d, a, b, c, x[i + 14], 9, -1019803690);
			c = gg(c, d, a, b, x[i + 3], 14, -187363961);
			b = gg(b, c, d, a, x[i + 8], 20, 1163531501);
			a = gg(a, b, c, d, x[i + 13], 5, -1444681467);
			d = gg(d, a, b, c, x[i + 2], 9, -51403784);
			c = gg(c, d, a, b, x[i + 7], 14, 1735328473);
			b = gg(b, c, d, a, x[i + 12], 20, -1926607734);
			a = hh(a, b, c, d, x[i + 5], 4, -378558);
			d = hh(d, a, b, c, x[i + 8], 11, -2022574463);
			c = hh(c, d, a, b, x[i + 11], 16, 1839030562);
			b = hh(b, c, d, a, x[i + 14], 23, -35309556);
			a = hh(a, b, c, d, x[i + 1], 4, -1530992060);
			d = hh(d, a, b, c, x[i + 4], 11, 1272893353);
			c = hh(c, d, a, b, x[i + 7], 16, -155497632);
			b = hh(b, c, d, a, x[i + 10], 23, -1094730640);
			a = hh(a, b, c, d, x[i + 13], 4, 681279174);
			d = hh(d, a, b, c, x[i + 0], 11, -358537222);
			c = hh(c, d, a, b, x[i + 3], 16, -722521979);
			b = hh(b, c, d, a, x[i + 6], 23, 76029189);
			a = hh(a, b, c, d, x[i + 9], 4, -640364487);
			d = hh(d, a, b, c, x[i + 12], 11, -421815835);
			c = hh(c, d, a, b, x[i + 15], 16, 530742520);
			b = hh(b, c, d, a, x[i + 2], 23, -995338651);
			a = ii(a, b, c, d, x[i + 0], 6, -198630844);
			d = ii(d, a, b, c, x[i + 7], 10, 1126891415);
			c = ii(c, d, a, b, x[i + 14], 15, -1416354905);
			b = ii(b, c, d, a, x[i + 5], 21, -57434055);
			a = ii(a, b, c, d, x[i + 12], 6, 1700485571);
			d = ii(d, a, b, c, x[i + 3], 10, -1894986606);
			c = ii(c, d, a, b, x[i + 10], 15, -1051523);
			b = ii(b, c, d, a, x[i + 1], 21, -2054922799);
			a = ii(a, b, c, d, x[i + 8], 6, 1873313359);
			d = ii(d, a, b, c, x[i + 15], 10, -30611744);
			c = ii(c, d, a, b, x[i + 6], 15, -1560198380);
			b = ii(b, c, d, a, x[i + 13], 21, 1309151649);
			a = ii(a, b, c, d, x[i + 4], 6, -145523070);
			d = ii(d, a, b, c, x[i + 11], 10, -1120210379);
			c = ii(c, d, a, b, x[i + 2], 15, 718787259);
			b = ii(b, c, d, a, x[i + 9], 21, -343485551);

			a = addme(a, olda);
			b = addme(b, oldb);
			c = addme(c, oldc);
			d = addme(d, oldd);

			i += 16;
		}
		return [a, b, c, d];
	}
}

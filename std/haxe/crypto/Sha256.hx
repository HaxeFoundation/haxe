/*
 * Copyright (C)2005-2016 Haxe Foundation
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
    Creates a Sha256 of a String.
*/
class Sha256 {

	public static function encode( s:String ) : String {
		#if php
		return untyped __call__("hash", "sha256", s);
		#else
		var sh = new Sha256();
		var h = sh.doEncode(str2blks(s), s.length*8);
		return sh.hex(h);
		#end
	}

	public static function make( b : haxe.io.Bytes ) : haxe.io.Bytes {
		#if php
		return haxe.io.Bytes.ofData(haxe.io.BytesData.ofString(untyped __call__("hash", "sha256", b.getData().toString(), true)));
		#else
		var h = new Sha256().doEncode(bytes2blks(b), b.length*8);
		var out = haxe.io.Bytes.alloc(32);
		var p = 0;
		for( i in 0...8 ) {
			out.set(p++,h[i]>>>24);
			out.set(p++,(h[i]>>16)&0xFF);
			out.set(p++,(h[i]>>8)&0xFF);
			out.set(p++,h[i]&0xFF);
		}
		return out;
		#end
	}

	public function new() {
	}

	function doEncode( m : Array<Int>, l : Int ) : Array<Int> {
		var K : Array<Int> = [
			0x428A2F98,0x71374491,0xB5C0FBCF,0xE9B5DBA5,0x3956C25B,
			0x59F111F1,0x923F82A4,0xAB1C5ED5,0xD807AA98,0x12835B01,
			0x243185BE,0x550C7DC3,0x72BE5D74,0x80DEB1FE,0x9BDC06A7,
			0xC19BF174,0xE49B69C1,0xEFBE4786,0xFC19DC6,0x240CA1CC,
			0x2DE92C6F,0x4A7484AA,0x5CB0A9DC,0x76F988DA,0x983E5152,
			0xA831C66D,0xB00327C8,0xBF597FC7,0xC6E00BF3,0xD5A79147,
			0x6CA6351,0x14292967,0x27B70A85,0x2E1B2138,0x4D2C6DFC,
			0x53380D13,0x650A7354,0x766A0ABB,0x81C2C92E,0x92722C85,
			0xA2BFE8A1,0xA81A664B,0xC24B8B70,0xC76C51A3,0xD192E819,
			0xD6990624,0xF40E3585,0x106AA070,0x19A4C116,0x1E376C08,
			0x2748774C,0x34B0BCB5,0x391C0CB3,0x4ED8AA4A,0x5B9CCA4F,
			0x682E6FF3,0x748F82EE,0x78A5636F,0x84C87814,0x8CC70208,
			0x90BEFFFA,0xA4506CEB,0xBEF9A3F7,0xC67178F2
		];
		var HASH : Array<Int> = [
			0x6A09E667,0xBB67AE85,0x3C6EF372,0xA54FF53A,
			0x510E527F,0x9B05688C,0x1F83D9AB,0x5BE0CD19
		];

		var W = new Array<Int>();
		W[64] = 0;
		var a:Int,b:Int,c:Int,d:Int,e:Int,f:Int,g:Int,h:Int;
		var T1, T2;
		m[l >> 5] |= 0x80 << (24 - l % 32);
		m[((l + 64 >> 9) << 4) + 15] = l;
		var i : Int = 0;
		while ( i < m.length ) {
			a = HASH[0]; b = HASH[1]; c = HASH[2]; d = HASH[3]; e = HASH[4]; f = HASH[5]; g = HASH[6]; h = HASH[7];
			for ( j in 0...64 ) {
				if (j < 16)
					W[j] = m[j + i];
				else
					W[j] = safeAdd(safeAdd(safeAdd(Gamma1256(W[j - 2]), W[j - 7]), Gamma0256(W[j - 15])), W[j - 16]);
				T1 = safeAdd(safeAdd(safeAdd(safeAdd(h, Sigma1256(e)), Ch(e, f, g)), K[j]), W[j]);
				T2 = safeAdd(Sigma0256(a), Maj(a, b, c));
				h = g; g = f; f = e; e = safeAdd(d, T1); d = c; c = b; b = a; a = safeAdd(T1, T2);
			}
			HASH[0] = safeAdd(a, HASH[0]);
			HASH[1] = safeAdd(b, HASH[1]);
			HASH[2] = safeAdd(c, HASH[2]);
			HASH[3] = safeAdd(d, HASH[3]);
			HASH[4] = safeAdd(e, HASH[4]);
			HASH[5] = safeAdd(f, HASH[5]);
			HASH[6] = safeAdd(g, HASH[6]);
			HASH[7] = safeAdd(h, HASH[7]);
			i += 16;
		}
		return HASH;
	}

	/*
		Convert a string to a sequence of 16-word blocks, stored as an array.
		Append padding bits and the length, as described in the SHA1 standard.
	 */
	static function str2blks( s :String ) : Array<Int> {
#if !(neko || cpp || php)
		var s = haxe.io.Bytes.ofString(s);
#end
		var nblk = ((s.length + 8) >> 6) + 1;
		var blks = new Array<Int>();

		for (i in 0...nblk*16)
			blks[i] = 0;
		for (i in 0...s.length){
			var p = i >> 2;
			blks[p] |= #if !(neko || cpp || php) s.get(i) #else s.charCodeAt(i) #end << (24 - ((i & 3) << 3));
		}
		var i = s.length;
		var p = i >> 2;
		blks[p] |= 0x80 << (24 - ((i & 3) << 3));
		blks[nblk * 16 - 1] = s.length * 8;
		return blks;
	}

	static function bytes2blks( b : haxe.io.Bytes ) : Array<Int> {
		var nblk = ((b.length + 8) >> 6) + 1;
		var blks = new Array<Int>();

		for (i in 0...nblk*16)
			blks[i] = 0;
		for (i in 0...b.length){
			var p = i >> 2;
			blks[p] |= b.get(i) << (24 - ((i & 3) << 3));
		}
		var i = b.length;
		var p = i >> 2;
		blks[p] |= 0x80 << (24 - ((i & 3) << 3));
		blks[nblk * 16 - 1] = b.length * 8;
		return blks;
	}

	function S(X, n) {
		return ( X >>> n ) | (X << (32 - n));
	}

	function R(X, n) {
		return ( X >>> n );
	}

	function Ch(x, y, z) {
		return ((x & y) ^ ((~x) & z));
	}

	function Maj(x, y, z) {
		return ((x & y) ^ (x & z) ^ (y & z));
	}

	function Sigma0256(x) {
		return (S(x, 2) ^ S(x, 13) ^ S(x, 22));
	}

	function Sigma1256(x) {
		return (S(x, 6) ^ S(x, 11) ^ S(x, 25));
	}

	function Gamma0256(x) {
		return (S(x, 7) ^ S(x, 18) ^ R(x, 3));
	}

	function Gamma1256(x) {
		return (S(x, 17) ^ S(x, 19) ^ R(x, 10));
	}

	function safeAdd(x, y) {
		var lsw = (x & 0xFFFF) + (y & 0xFFFF);
		var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
		return (msw << 16) | (lsw & 0xFFFF);
	}

	function hex( a : Array<Int> ){
		var str = "";
		for( num in a ) {
			str += StringTools.hex(num, 8);
		}
		return str.toLowerCase();
	}

}

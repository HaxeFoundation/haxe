/*
 * Copyright (c) 2005-2010, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe.crypto;

class Sha1 {

	public static function encode( s:String ) : String {
		#if php
		return untyped __call__("sha1", s);
		#else
		var sh = new Sha1();
		var h = sh.doEncode(str2blks(s));
		return sh.hex(h);
		#end
	}

	public static function make( b : haxe.io.Bytes ) : haxe.io.Bytes {
		#if php
		return haxe.io.Bytes.ofData(untyped __call__("sha1", b.getData(), true));
		#else
		var h = new Sha1().doEncode(bytes2blks(b));
		var out = haxe.io.Bytes.alloc(20);
		var p = 0;
		for( i in 0...5 ) {
			out.set(p++,h[i]>>>24);
			out.set(p++,(h[i]>>16)&0xFF);
			out.set(p++,(h[i]>>8)&0xFF);
			out.set(p++,h[i]&0xFF);
		}
		return out;
		#end
	}

	#if !php

	function new() {
	}

	function doEncode( x : Array<Int> ) : Array<Int> {
		var w = new Array<Int>();

		var a = 0x67452301;
		var b = 0xEFCDAB89;
		var c = 0x98BADCFE;
		var d = 0x10325476;
		var e = 0xC3D2E1F0;

		var i = 0;
		while( i < x.length ) {
			var olda = a;
			var oldb = b;
			var oldc = c;
			var oldd = d;
			var olde = e;

			var j = 0;
			while( j < 80 ) {
				if(j < 16)
					w[j] = x[i + j];
				else
					w[j] = rol(w[j-3] ^ w[j-8] ^ w[j-14] ^ w[j-16], 1);
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
		return [a,b,c,d,e];
	}


	/*
		Convert a string to a sequence of 16-word blocks, stored as an array.
		Append padding bits and the length, as described in the SHA1 standard.
	 */
	static function str2blks( s :String ) : Array<Int> {
		var nblk = ((s.length + 8) >> 6) + 1;
		var blks = new Array<Int>();

		for (i in 0...nblk*16)
			blks[i] = 0;
		for (i in 0...s.length){
			var p = i >> 2;
			blks[p] |= s.charCodeAt(i) << (24 - ((i & 3) << 3));
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

	/**
		Bitwise rotate a 32-bit number to the left
	 */
	inline function rol( num : Int, cnt : Int ) : Int {
		return (num << cnt) | (num >>> (32 - cnt));
	}

	/**
		Perform the appropriate triplet combination function for the current iteration
	*/
	function ft( t : Int, b : Int, c : Int, d : Int ) : Int {
		if ( t < 20 ) return (b & c) | ((~b) & d);
		if ( t < 40 ) return b ^ c ^ d;
		if ( t < 60 ) return (b & c) | (b & d) | (c & d);
		return b ^ c ^ d;
	}

	/**
		Determine the appropriate additive constant for the current iteration
	*/
	function kt( t : Int ) : Int {
		if( t < 20)
			return 0x5A827999;
		if ( t < 40)
			return 0x6ED9EBA1;
		if (t < 60)
			return 0x8F1BBCDC;
		return 0xCA62C1D6;
	}

	function hex( a : Array<Int> ){
		var str = "";
		var hex_chr = "0123456789abcdef";
		for( num in a ) {
			var j = 7;
			while( j >= 0 ) {
				str += hex_chr.charAt( (num >>> (j<<2)) & 0xF );
				j--;
			}
		}
		return str;
	}

	#end

}

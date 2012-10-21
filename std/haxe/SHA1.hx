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
package haxe;

#if !haxe3

import haxe.Int32;

class SHA1 {

	static var hex_chr = "0123456789abcdef";

	inline static function newInt32( left:Int, right:Int ) : Int32 {
		var result = Int32.ofInt(left);
		result = Int32.shl(result, 16);
		return Int32.add(result, Int32.ofInt(right));
	}

	public static function encode( s:String ) : String {
		var x = str2blks_SHA1( s );
		var w = new Array<Int32>();

		var a = newInt32(0x6745, 0x2301);
		var b = newInt32(0xEFCD, 0xAB89);
		var c = newInt32(0x98BA, 0xDCFE);
		var d = newInt32(0x1032, 0x5476);
		var e = newInt32(0xC3D2, 0xE1F0);

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
					w[j] = rol(Int32.xor(Int32.xor(Int32.xor(w[j-3], w[j-8]), w[j-14]), w[j-16]), 1);
				var t = add(add(rol(a, 5), ft(Int32.ofInt(j), b, c, d)), add(add(e, w[j]), kt(Int32.ofInt(j))));
				e = d;
				d = c;
				c = rol(b, 30);
				b = a;
				a = t;

				j++;
			}
			a = add( a, olda );
			b = add( b, oldb );
			c = add( c, oldc );
			d = add( d, oldd );
			e = add( e, olde );
			i += 16;
		}
		return hex(a) + hex(b) + hex(c) + hex(d) + hex(e);
	}

	static function hex( num : Int32 ) : String {
		var str = "";
		var j = 7;
		while( j >= 0 ) {
			str += hex_chr.charAt( Int32.toInt( Int32.and((Int32.shr(num, j*4)), Int32.ofInt(0x0F)) ) );
			j--;
		}
		return str;
	}

	/**
		Convert a string to a sequence of 16-word blocks, stored as an array.
		Append padding bits and the length, as described in the SHA1 standard.
	 */
	static function str2blks_SHA1( s :String ) : Array<Int32> {
		var nblk = ((s.length + 8) >> 6) + 1;
		var blks = new Array<Int32>();

		for (i in 0...nblk*16)
			blks[i] = Int32.ofInt(0);
		for (i in 0...s.length){
			var p = i >> 2;
			var c = Int32.ofInt(s.charCodeAt(i));
			blks[p] = Int32.or(blks[p], Int32.shl(c, (24 - (i % 4) * 8)));
		}
		var i = s.length;
		var p = i >> 2;
		blks[p] = Int32.or(blks[p], Int32.shl(Int32.ofInt(0x80), (24 - (i % 4) * 8)));
		blks[nblk * 16 - 1] = Int32.ofInt(s.length * 8);
		return blks;
	}

	/**
		Add integers, wrapping at 2^32.
	 */
	static function add( x : Int32, y : Int32 ) : Int32 {
		var lsw = Int32.add(Int32.and(x, Int32.ofInt(0xFFFF)), Int32.and(y, Int32.ofInt(0xFFFF)));
		var msw = Int32.add(Int32.add(Int32.shr(x, 16), Int32.shr(y, 16)), Int32.shr(lsw, 16));
		return Int32.or(Int32.shl(msw, 16), Int32.and(lsw, Int32.ofInt(0xFFFF)));
	}

	/**
		Bitwise rotate a 32-bit number to the left
	 */
	static function rol( num : Int32, cnt : Int ) : Int32 {
		return Int32.or(Int32.shl(num, cnt), Int32.ushr(num, (32 - cnt)));
	}

	/**
		Perform the appropriate triplet combination function for the current iteration
	*/
	static function ft( t : Int32, b : Int32, c : Int32, d : Int32 ) : Int32 {
		if (Int32.compare(t, Int32.ofInt(20)) <0) return Int32.or(Int32.and(b, c), Int32.and((Int32.complement(b)), d));
		if (Int32.compare(t, Int32.ofInt(40)) <0) return Int32.xor(Int32.xor(b, c), d);
		if (Int32.compare(t, Int32.ofInt(60)) <0) return Int32.or(Int32.or(Int32.and(b, c), Int32.and(b, d)), Int32.and(c, d));
		return Int32.xor(Int32.xor(b, c), d);
	}

	/**
		Determine the appropriate additive constant for the current iteration
	*/
	static function kt( t : Int32 ) : Int32 {
		if (Int32.compare(t,Int32.ofInt(20)) < 0)
			return newInt32(0x5A82, 0x7999);
		if (Int32.compare(t,Int32.ofInt(40)) < 0)
			return newInt32(0x6ed9, 0xeba1);
		if (Int32.compare(t,Int32.ofInt(60)) < 0)
			return newInt32(0x8f1b, 0xbcdc);
		return newInt32(0xca62, 0xc1d6);
	}

}

#end
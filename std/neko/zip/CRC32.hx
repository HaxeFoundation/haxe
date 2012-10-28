/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package neko.zip;

#if !haxe3

import haxe.Int32;

class CRC32 {

	/*
	 *  Function computes CRC32 code of a given string.
	 *  Warning: returns Int32 as result uses all 32 bits
	 *  UTF - 8 coding is not supported
	 */
	public static function encode(str : haxe.io.Bytes) : Int32 {
		var init = Int32.make(0xFFFF, 0xFFFF);
		var polynom = Int32.make(0xEDB8, 0x8320);
		var crc = init;
		var s = str.getData();
		for( i in 0...str.length ) {
			var tmp = Int32.and( Int32.xor(crc,untyped __dollar__sget(s,i)), cast 0xFF );
			for( j in 0...8 ) {
				if( Int32.and(tmp,cast 1) == cast 1 )
					tmp = Int32.xor(Int32.ushr(tmp,1),polynom);
				else
					tmp = Int32.ushr(tmp,1);
			}
			crc = Int32.xor(Int32.ushr(crc,8), tmp);
		}
		return Int32.xor(crc, init);
	}
}

#end
/*
 * Copyright (C)2005-2018 Haxe Foundation
 *
 * Permission is hereby granted, free of Charge, to any person obtaining a
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
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERChANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package haxe.crypto;

/**
    Creates a Sha224 of a String.
*/
class Sha224 {

    public static function encode( s:String ) : String {
        var sh = new Sha224();
        var h = sh.doEncode(s, s.length*8);
        return sh.hex(h);
    }

    public static function make( b : haxe.io.Bytes ) : haxe.io.Bytes {
        var h = new Sha224().doEncode(b.toString(), b.length*8);
        var out = haxe.io.Bytes.alloc(28);
        var p = 0;
        for( i in 0...8 ) {
            out.set(p++,h[i]>>>24);
            out.set(p++,(h[i]>>16)&0xFF);
            out.set(p++,(h[i]>>8)&0xFF);
            out.set(p++,h[i]&0xFF);
        }
        return out;
    }

    public function new() {
    }

    function doEncode( str : String, strlen : Int ) : Array<Int> {
        var K : Array<Int> = [
            0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5,
            0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
            0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
            0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
            0xE49B69C1, 0xEFBE4786, 0xFC19DC6 , 0x240CA1CC,
            0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
            0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7,
            0xC6E00BF3, 0xD5A79147, 0x6CA6351 , 0x14292967,
            0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
            0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
            0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3,
            0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
            0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5,
            0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
            0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
            0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2
        ];
        var HASH : Array<Int> = [
            0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939,
            0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4
        ];
        var W = new Array<Int>();
        W[64] = 0;

        var a:Int,b:Int,c:Int,d:Int,e:Int,f:Int,g:Int,h:Int,i:Int,j:Int;
        var T1, T2;
        var i : Int = 0;
        var blocks : Array<Int> = str2blks(str);
        blocks[strlen >> 5] |= 0x80 << (24 - strlen % 32);
        blocks[((strlen + 64 >> 9) << 4) + 15] = strlen;

        while ( i < blocks.length ) {
            a = HASH[0];
            b = HASH[1];
            c = HASH[2];
            d = HASH[3];
            e = HASH[4];
            f = HASH[5];
            g = HASH[6];
            h = HASH[7];

            for ( j in 0...64 ) {
                if (j < 16) {
                    W[j] = blocks[j + i];
                } else {
                    W[j] = safeAdd(safeAdd(safeAdd(Gamma1(W[j - 2]), W[j - 7]), Gamma0(W[j - 15])), W[j - 16]);
                }

                T1 = safeAdd(safeAdd(safeAdd(safeAdd(h, Sigma1(e)), Ch(e, f, g)), K[j]), W[j]);
                T2 = safeAdd(Sigma0(a), Maj(a, b, c));

                h = g;
                g = f;
                f = e;
                e = safeAdd(d, T1);
                d = c;
                c = b;
                b = a;
                a = safeAdd(T1, T2);
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

    extern
    inline static function safeAdd(x, y) {
        var lsw = (x & 0xFFFF) + (y & 0xFFFF);
        var msw = (x >>> 16) + (y >>> 16) + (lsw >>> 16);
        return ((msw & 0xFFFF) << 16) | (lsw & 0xFFFF);
    }

    // ++
    extern
    inline function ROTR(X, n) {
        return ( X >>> n ) | (X << (32 - n));
    }

    // ++
    extern
    inline function SHR(X, n) {
        return ( X >>> n );
    }

    // ++
    extern
    inline function Ch(x, y, z) {
        return ((x & y) ^ ((~x) & z));
    }

    // ++
    extern
    inline function Maj(x, y, z) {
        return ((x & y) ^ (x & z) ^ (y & z));
    }

    extern
    inline function Sigma0(x) {
        return ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22);
    }

    extern
    inline function Sigma1(x) {
        return ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25);
    }

    extern
    inline function Gamma0(x) {
        return ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3);
    }

    extern
    inline function Gamma1(x) {
        return ROTR(x, 17) ^ ROTR(x, 19) ^ SHR(x, 10);
    }

    function hex( a : Array<Int> ){
        var str = "";
        for( num in a ) {
            str += StringTools.hex(num, 8);
        }
        return str.substring(0, 56).toLowerCase();
    }

}

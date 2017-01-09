/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.zip;
import haxe.zip.Huffman;
import haxe.crypto.Adler32;

private class Window {

	public static inline var SIZE = 1 << 15;
	public static inline var BUFSIZE = 1 << 16;

	public var buffer : haxe.io.Bytes;
	public var pos : Int;
	var crc : Adler32;

	public function new(hasCrc) {
		buffer = haxe.io.Bytes.alloc(BUFSIZE);
		pos = 0;
		if( hasCrc ) crc = new Adler32();
	}

	public function slide() {
		if( crc != null ) crc.update(buffer,0,SIZE);
		var b = haxe.io.Bytes.alloc(BUFSIZE);
		pos -= SIZE;
		b.blit(0,buffer,SIZE,pos);
		buffer = b;
	}

	public function addBytes(b,p,len) {
		if( pos + len > BUFSIZE ) slide();
		buffer.blit(pos,b,p,len);
		pos += len;
	}

	public function addByte(c) {
		if( pos == BUFSIZE ) slide();
		buffer.set(pos,c);
		pos++;
	}

	public function getLastChar() {
		return buffer.get(pos - 1);
	}

	public function available() {
		return pos;
	}

	public function checksum() {
		if( crc != null ) crc.update(buffer,0,pos);
		return crc;
	}

}

private enum State {
	Head;
	Block;
	CData;
	Flat;
	Crc;
	Dist;
	DistOne;
	Done;
}

/**
	A pure Haxe implementation of the ZLIB Inflate algorithm which allows reading compressed data without any platform-specific support.
**/
class InflateImpl {

	static var LEN_EXTRA_BITS_TBL = [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,-1,-1];
	static var LEN_BASE_VAL_TBL = [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258];
	static var DIST_EXTRA_BITS_TBL = [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,-1,-1];
	static var DIST_BASE_VAL_TBL = [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577];
	static var CODE_LENGTHS_POS = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15];

	var nbits : Int;
	var bits : Int;
	var state : State;
	var final : Bool;
	var huffman : Huffman;
	var huffdist : Null<Huffman>;
	var htools : HuffTools;
	var len : Int;
	var dist : Int;
	var needed : Int;
	var output : haxe.io.Bytes;
	var outpos : Int;
	var input : haxe.io.Input;
	var lengths : Array<Int>;
	var window : Window;

	static var FIXED_HUFFMAN = null;

	public function new( i, ?header = true, ?crc = true ) {
		final = false;
		htools = new HuffTools();
		huffman = buildFixedHuffman();
		huffdist = null;
		len = 0;
		dist = 0;
		state = header ? Head : Block;
		input = i;
		bits = 0;
		nbits = 0;
		needed = 0;
		output = null;
		outpos = 0;
		lengths = new Array();
		for( i in 0...19 )
			lengths.push(-1);
		window = new Window(crc);
	}

	function buildFixedHuffman() {
		if( FIXED_HUFFMAN != null )
			return FIXED_HUFFMAN;
		var a = new Array();
		for( n in 0...288 )
			a.push(if( n <= 143 ) 8 else if( n <= 255 ) 9 else if( n <= 279 ) 7 else 8);
		FIXED_HUFFMAN = htools.make(a,0,288,10);
		return FIXED_HUFFMAN;
	}

	public function readBytes(b,pos,len) {
		needed = len;
		outpos = pos;
		output = b;
		if( len > 0 )
			while( inflateLoop() ) {
			}
		return len - needed;
	}

	function getBits(n) {
		while( nbits < n ) {
			bits |= input.readByte() << nbits;
			nbits += 8;
		}
		var b = bits & ((1 << n) - 1);
		nbits -= n;
		bits >>= n;
		return b;
	}

	function getBit() {
		if( nbits == 0 ) {
			nbits = 8;
			bits = input.readByte();
		}
		var b = bits & 1 == 1;
		nbits--;
		bits >>= 1;
		return b;
	}

	function getRevBits(n) {
		return if( n == 0 )
			0
		else if( getBit() )
			(1 << (n - 1)) | getRevBits(n-1)
		else
			getRevBits(n-1);
	}

	function resetBits() {
		bits = 0;
		nbits = 0;
	}

	function addBytes(b,p,len) {
		window.addBytes(b,p,len);
		output.blit(outpos,b,p,len);
		needed -= len;
		outpos += len;
	}

	function addByte(b) {
		window.addByte(b);
		output.set(outpos,b);
		needed--;
		outpos++;
	}

	function addDistOne(n) {
		var c = window.getLastChar();
		for( i in 0...n )
			addByte(c);
	}

	function addDist(d,len) {
		addBytes(window.buffer,window.pos - d,len);
	}

	function applyHuffman(h) {
		return switch(h) {
		case Found(n): n;
		case NeedBit(a,b): applyHuffman(getBit()?b:a);
		case NeedBits(n,tbl): applyHuffman(tbl[getBits(n)]);
		}
	}

	function inflateLengths(a,max) {
		var i = 0;
		var prev = 0;
		while( i < max ) {
			var n = applyHuffman(huffman);
			switch( n ) {
			case 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15:
				prev = n;
				a[i] = n;
				i++;
			case 16:
				var end = i + 3 + getBits(2);
				if( end > max ) throw "Invalid data";
				while( i < end ) {
					a[i] = prev;
					i++;
				}
			case 17:
				i += 3 + getBits(3);
				if( i > max ) throw "Invalid data";
			case 18:
				i += 11 + getBits(7);
				if( i > max ) throw "Invalid data";
			default:
				throw "Invalid data";
			}
		}
	}

	function inflateLoop() {
		switch( state ) {
		case Head:
			var cmf = input.readByte();
			var cm = cmf & 15;
			var cinfo = cmf >> 4;
			if( cm != 8 ) throw "Invalid data";
			var flg = input.readByte();
			//var fcheck = flg & 31;
			var fdict = flg & 32 != 0;
			//var flevel = flg >> 6;
			if( ((cmf << 8) + flg) % 31 != 0 ) throw "Invalid data";
			if( fdict ) throw "Unsupported dictionary";
			state = Block;
			return true;
		case Crc:
			var calc = window.checksum();
			if( calc == null ) {
				state = Done;
				return true;
			}
			var crc = Adler32.read(input);
			if( !calc.equals(crc) ) throw "Invalid CRC";
			state = Done;
			return true;
		case Done:
			// nothing
			return false;
		case Block:
			final = getBit();
			switch( getBits(2) ) {
			case 0: // no compression
				len = input.readUInt16();
				var nlen = input.readUInt16();
				if( nlen != 0xFFFF - len ) throw "Invalid data";
				state = Flat;
				var r = inflateLoop();
				resetBits();
				return r;
			case 1: // fixed Huffman
				huffman = buildFixedHuffman();
				huffdist = null;
				state = CData;
				return true;
			case 2: // dynamic Huffman
				var hlit = getBits(5) + 257;
				var hdist = getBits(5) + 1;
				var hclen = getBits(4) + 4;
				for( i in 0...hclen )
					lengths[CODE_LENGTHS_POS[i]] = getBits(3);
				for( i in hclen...19 )
					lengths[CODE_LENGTHS_POS[i]] = 0;
				huffman = htools.make(lengths,0,19,8);
				var lengths = new Array();
				for( i in 0...hlit + hdist )
					lengths.push(0);
				inflateLengths(lengths,hlit + hdist);
				huffdist = htools.make(lengths,hlit,hdist,16);
				huffman = htools.make(lengths,0,hlit,16);
				state = CData;
				return true;
			default:
				throw "Invalid data";
			}
		case Flat:
			var rlen = (len < needed) ? len : needed;
			var bytes = input.read(rlen);
			len -= rlen;
			addBytes(bytes,0,rlen);
			if( len == 0 ) state = final ? Crc : Block;
			return needed > 0;
		case DistOne:
			var rlen = (len < needed) ? len : needed;
			addDistOne(rlen);
			len -= rlen;
			if( len == 0 ) state = CData;
			return needed > 0;
		case Dist:
			while( len > 0 && needed > 0 ) {
				var rdist = (len < dist) ? len : dist;
				var rlen = (needed < rdist) ? needed : rdist;
				addDist(dist,rlen);
				len -= rlen;
			}
			if( len == 0 ) state = CData;
			return needed > 0;
		case CData:
			var n = applyHuffman(huffman);
			if( n < 256 ) {
				addByte(n);
				return needed > 0;
			} else if( n == 256 ) {
				state = final ? Crc : Block;
				return true;
			} else {
				n -= 257;
				var extra_bits = LEN_EXTRA_BITS_TBL[n];
				if( extra_bits == -1 ) throw "Invalid data";
				len = LEN_BASE_VAL_TBL[n] + getBits(extra_bits);
				var dist_code = if( huffdist == null ) getRevBits(5) else applyHuffman(huffdist);
				extra_bits = DIST_EXTRA_BITS_TBL[dist_code];
				if( extra_bits == -1 ) throw "Invalid data";
				dist = DIST_BASE_VAL_TBL[dist_code] + getBits(extra_bits);
				if( dist > window.available() ) throw "Invalid data";
				state = (dist == 1) ? DistOne : Dist;
				return true;
			}
		}
	}

	public static function run( i : haxe.io.Input, ?bufsize = 65536 ) {
		var buf = haxe.io.Bytes.alloc(bufsize);
		var output = new haxe.io.BytesBuffer();
		var inflate = new InflateImpl(i);
		while( true ) {
			var len = inflate.readBytes(buf,0,bufsize);
			output.addBytes(buf,0,len);
			if( len < bufsize )
				break;
		}
		return output.getBytes();
	}

}

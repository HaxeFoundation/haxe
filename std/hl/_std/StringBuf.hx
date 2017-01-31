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
@:coreApi class StringBuf {

	var b : hl.Bytes;
	var size : Int;
	var pos : Int;
	public var length(get,never) : Int;

	public function new() : Void {
		pos = 0;
		size = 8; // ensure 4 bytes expand for addChar()
		b = new hl.Bytes(size);
	}

	inline function get_length() : Int {
		return pos >> 1;
	}

	inline function __expand( need : Int ) : Void {
		var nsize = (size * 3) >> 1;
		if( need > nsize ) nsize = need;
		var b2 = new hl.Bytes(nsize);
		b2.blit(0, b, 0, pos);
		b = b2;
		size = nsize;
	}

	inline function __add( bytes : hl.Bytes, spos : Int, ssize : Int ) : Void {
		if( pos + ssize > size ) __expand(pos + ssize);
		b.blit(pos, bytes, spos, ssize);
		pos += ssize;
	}

	public function add<T>( x : T ) : Void {
		var slen = 0;
		var sbytes = hl.Bytes.fromValue(x, new hl.Ref(slen));
		__add(sbytes, 0, slen<<1);
	}

	public function addSub( s : String, pos : Int, ?len : Int ) : Void @:privateAccess {
		if( pos < 0 ) pos = 0;
		if( pos >= s.length ) return;
		var slen : Int;
		if( len == null ) slen = s.length - pos else {
			slen = len;
			if( pos + slen > s.length ) slen = s.length - pos;
			if( slen <= 0 ) return;
		}
		__add(s.bytes, pos << 1, slen << 1);
	}

	public function addChar( c : Int ) : Void {
		if( c >= 0 && c < 0x10000 ) {
			if( c >= 0xD800 && c <= 0xDFFF ) throw "Invalid unicode char " + c;
			if( pos + 2 > size ) __expand(0);
			b.setUI16(pos, c);
			pos += 2;
		} else if( c < 0x110000 ) {
			if( pos + 4 > size ) __expand(0);
			c -= 0x10000;
			b.setUI16(pos, (c >> 10) + 0xD800);
			b.setUI16(pos + 2, (c & 1023) + 0xDC00);
			pos += 4;
		} else
			throw "Invalid unicode char " + c;
	}

	public function toString() : String {
		if( pos+2 > size ) __expand(0);
		b.setUI16(pos,0);
		return @:privateAccess String.__alloc__(b, pos>>1);
	}

}

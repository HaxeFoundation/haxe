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
package haxe.io;

@:coreApi
class BytesBuffer {

	var b : hl.Bytes;
	var pos : Int;
	var size : Int;

	public var length(get,never) : Int;

	public function new() {
		pos = 0;
		size = 16; // ensure increment of 8
		b = new hl.Bytes(size);
	}

	inline function get_length() : Int {
		return pos;
	}

	public inline function addByte( byte : Int ) : Void {
		if( pos == size ) __expand(0);
		b[pos++] = byte;
	}

	function __expand( req : Int ) : Void {
		var nsize = (size * 3) >> 1;
		if( nsize < req ) nsize = req;
		var b2 = new hl.Bytes(nsize);
		b2.blit(0, b, 0, pos);
		b = b2;
		size = nsize;
	}

	function __add( b : hl.Bytes, bpos : Int, blen : Int ) : Void {
		if( pos + blen > size ) __expand(pos+blen);
		this.b.blit(pos, b, bpos, blen);
		pos += blen;
	}

	public inline function add( src : Bytes ) : Void {
		__add(@:privateAccess src.b, 0, src.length);
	}

	public inline function addString( v : String ) : Void {
		var len = 0;
		@:privateAccess __add(v.bytes.utf16ToUtf8(0, len), 0, len);
	}

	public inline function addInt32( v : Int ) : Void {
		if( pos + 4 > size ) __expand(0);
		b.setI32(pos, v);
		pos += 4;
	}

	public inline function addInt64( v : haxe.Int64 ) : Void {
		if( pos + 8 > size ) __expand(0);
		b.setI32(pos, v.low);
		b.setI32(pos + 4, v.high);
		pos += 8;
	}

	public inline function addFloat( v : Float ) : Void {
		if( pos + 4 > size ) __expand(0);
		b.setF32(pos, v);
		pos += 4;
	}

	public inline function addDouble( v : Float ) : Void {
		if( pos + 8 > size ) __expand(0);
		b.setF64(pos, v);
		pos += 8;
	}

	public inline function addBytes( src : Bytes, pos : Int, len : Int ) : Void {
		if( pos < 0 || len < 0 || pos + len > src.length ) throw Error.OutsideBounds;
		__add(@:privateAccess src.b, pos, len);
	}

	public function getBytes() : Bytes {
		return @:privateAccess new haxe.io.Bytes(b, pos);
	}

}

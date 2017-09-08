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

class BytesBuffer {

	var buffer : js.html.ArrayBuffer;
	var view : js.html.DataView;
	var u8 : js.html.Uint8Array;
	var pos : Int;
	var size : Int;

	public var length(get,never) : Int;

	public function new() {
		pos = 0;
		size = 0;
	}

	inline function get_length() : Int {
		return pos;
	}

	public function addByte( byte : Int ) {
		if( pos == size ) grow(1);
		view.setUint8(pos++, byte);
	}

	public function add( src : Bytes ) {
		if( pos + src.length > size ) grow(src.length);
		if( size == 0 ) return;
		var sub = new js.html.Uint8Array(@:privateAccess src.b.buffer, @:privateAccess src.b.byteOffset, src.length);
		u8.set(sub, pos);
		pos += src.length;
	}

	public function addString( v : String ) {
		add(Bytes.ofString(v));
	}

	public function addInt32( v : Int ) {
		if( pos + 4 > size ) grow(4);
		view.setInt32(pos, v, true);
		pos += 4;
	}

	public function addInt64( v : haxe.Int64 ) {
		if( pos + 8 > size ) grow(8);
		view.setInt32(pos, v.low, true);
		view.setInt32(pos + 4, v.high, true);
		pos += 8;
	}

	public function addFloat( v : Float ) {
		if( pos + 4 > size ) grow(4);
		view.setFloat32(pos, v, true);
		pos += 4;
	}

	public function addDouble( v : Float ) {
		if( pos + 8 > size ) grow(8);
		view.setFloat64(pos, v, true);
		pos += 8;
	}

	public function addBytes( src : Bytes, pos : Int, len : Int ) {
		if( pos < 0 || len < 0 || pos + len > src.length ) throw Error.OutsideBounds;
		if( this.pos + len > size ) grow(len);
		if( size == 0 ) return;
		var sub = new js.html.Uint8Array(@:privateAccess src.b.buffer, @:privateAccess src.b.byteOffset + pos, len);
		u8.set(sub, this.pos);
		this.pos += len;
	}

	function grow( delta : Int ) {
		var req = pos + delta;
		var nsize = size == 0 ? 16 : size;
		while( nsize < req )
			nsize = (nsize * 3) >> 1;
		var nbuf = new js.html.ArrayBuffer(nsize);
		var nu8 = new js.html.Uint8Array(nbuf);
		if( size > 0 )
			nu8.set(u8);
		size = nsize;
		buffer = nbuf;
		u8 = nu8;
		view = new js.html.DataView(buffer);
	}

	public function getBytes() : Bytes @:privateAccess {
		if( size == 0 )
			return haxe.io.Bytes.alloc(0);
		var b = new Bytes(buffer);
		b.length = pos;
		return b;
	}

}

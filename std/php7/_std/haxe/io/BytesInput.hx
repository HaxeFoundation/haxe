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

class BytesInput extends Input {
	var b : BytesData;
	var pos : Int;
	var len : Int;
	var totlen : Int;

	/** The current position in the stream in bytes. */
	public var position(get,set) : Int;

	/** The length of the stream in bytes. */
	public var length(get,never) : Int;

	public function new( b : Bytes, ?pos : Int, ?len : Int ) {
		if( pos == null ) pos = 0;
		if( len == null ) len = b.length - pos;
		if( pos < 0 || len < 0 || pos + len > b.length ) throw Error.OutsideBounds;

		this.b = b.getData();
		this.pos = pos;
		this.len = len;
		this.totlen = len;
	}

	inline function get_position() : Int {
		return pos;
	}

	inline function get_length() : Int {
		return totlen;
	}

	function set_position( p : Int ) : Int {
		if( p < 0 ) p = 0;
		else if( p > length ) p = length;
		len = totlen - p;
		return pos = p;
	}

	public override function readByte() : Int {
		if( len == 0 ) throw new Eof();
		--len;
		return b[pos++];
	}

	public override function readBytes( buf : Bytes, pos, len ) : Int {
		if( pos < 0 || len < 0 || pos + len > buf.length )
			throw Error.OutsideBounds;
		if( this.len == 0 && len > 0 )
			throw new Eof();
		if( this.len < len )
			len = this.len;
		buf.getData().blit(pos, b, this.pos, len);
		this.pos += len;
		this.len -= len;

		return len;
	}
}

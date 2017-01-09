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

class BytesOutput extends Output {

	#if flash
	var b : flash.utils.ByteArray;
	#else
	var b : BytesBuffer;
	#end

	/** The length of the stream in bytes. **/
	public var length(get,never) : Int;

	public function new() {
		#if flash
		b = new flash.utils.ByteArray();
		b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#else
		b = new BytesBuffer();
		#end
		#if python
		bigEndian = false;
		#end
	}

	inline function get_length() : Int {
		return b.length;
	}

	override function writeByte(c) {
		#if flash
		b.writeByte(c);
		#else
		b.addByte(c);
		#end
	}

	override function writeBytes( buf : Bytes, pos, len ) : Int {
		#if flash
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw Error.OutsideBounds;
		b.writeBytes(buf.getData(),pos,len);
		#else
		b.addBytes(buf,pos,len);
		#end
		return len;
	}

	#if flash
	// optimized operations

	@:dox(hide)
	override function set_bigEndian(e) {
		bigEndian = e;
		b.endian = e ? flash.utils.Endian.BIG_ENDIAN : flash.utils.Endian.LITTLE_ENDIAN;
		return e;
	}

	@:dox(hide)
	override function writeFloat( f : Float ) {
		b.writeFloat(f);
	}

	@:dox(hide)
	override function writeDouble( f : Float ) {
		b.writeDouble(f);
	}

	@:dox(hide)
	override function writeInt8( x : Int ) {
		if( x < -0x80 || x >= 0x80 )
			throw Error.Overflow;
		b.writeByte(x);
	}

	@:dox(hide)
	override function writeInt16( x : Int ) {
		if( x < -0x8000 || x >= 0x8000 ) throw Error.Overflow;
		b.writeShort(x);
	}

	@:dox(hide)
	override function writeUInt16( x : Int ) {
		if( x < 0 || x >= 0x10000 ) throw Error.Overflow;
		b.writeShort(x);
	}

	@:dox(hide)
	override function writeInt32( x : Int ) {
		b.writeInt(x);
	}

	@:dox(hide)
	override function prepare( size : Int ) {
		if( size > 0 ) b[size-1] = b[size-1];
	}

	@:dox(hide)
	override function writeString( s : String ) {
		b.writeUTFBytes(s);
	}

	#end

	/**
		Returns the `Bytes` of this output.

		This function should not be called more than once on a given
		`BytesOutput` instance.
	**/
	public function getBytes() : Bytes {
		#if flash
		var bytes = b;
		b = null;
		return untyped new Bytes(bytes.length,bytes);
		#else
		return b.getBytes();
		#end
	}

}

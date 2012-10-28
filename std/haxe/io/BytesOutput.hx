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
package haxe.io;

class BytesOutput extends Output {

	#if flash9
	var b : flash.utils.ByteArray;
	#else
	var b : BytesBuffer;
	#end

	public function new() {
		#if flash9
		b = new flash.utils.ByteArray();
		b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#else
		b = new BytesBuffer();
		#end
	}

	override function writeByte(c) {
		#if flash9
		b.writeByte(c);
		#else
		b.addByte(c);
		#end
	}

	override function writeBytes( buf : Bytes, pos, len ) : Int {
		#if flash9
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw Error.OutsideBounds;
		b.writeBytes(buf.getData(),pos,len);
		#else
		b.addBytes(buf,pos,len);
		#end
		return len;
	}

	#if flash9
	// optimized operations

	override function setEndian(e) {
		bigEndian = e;
		b.endian = e ? flash.utils.Endian.BIG_ENDIAN : flash.utils.Endian.LITTLE_ENDIAN;
		return e;
	}

	override function writeFloat( f : Float ) {
		b.writeFloat(f);
	}

	override function writeDouble( f : Float ) {
		b.writeDouble(f);
	}

	override function writeInt8( x : Int ) {
		if( x < -0x80 || x >= 0x80 )
			throw Error.Overflow;
		b.writeByte(x);
	}

	override function writeInt16( x : Int ) {
		if( x < -0x8000 || x >= 0x8000 ) throw Error.Overflow;
		b.writeShort(x);
	}

	override function writeUInt16( x : Int ) {
		if( x < 0 || x >= 0x10000 ) throw Error.Overflow;
		b.writeShort(x);
	}

	#if haxe3

	override function writeInt32( x : Int ) {
		b.writeInt(x);
	}

	#else

	override function writeInt31( x : Int ) {
		#if !neko
		if( x < -0x40000000 || x >= 0x40000000 ) throw Error.Overflow;
		#end
		b.writeInt(x);
	}

	override function writeUInt30( x : Int ) {
		if( x < 0 || x >= 0x40000000 ) throw Error.Overflow;
		b.writeInt(x);
	}

	override function writeInt32( x : haxe.Int32 ) {
		b.writeInt(cast x);
	}

	#end

	override function prepare( size : Int ) {
		if( size > 0 )
	#if cpp
			untyped b.__Resize(size);
	#else
			b[size-1] = b[size-1];
	#end
	}

	override function writeString( s : String ) {
		b.writeUTFBytes(s);
	}

	#end

	public function getBytes() : Bytes {
		#if flash9
		var bytes = b;
		b = null;
		return untyped new Bytes(bytes.length,bytes);
		#else
		return b.getBytes();
		#end
	}

}

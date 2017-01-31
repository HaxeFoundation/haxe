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

/**
	An Input is an abstract reader. See other classes in the `haxe.io` package
	for several possible implementations.

	All functions which read data throw `Eof` when the end of the stream
	is reached.
**/
class Input {

	/**
		Endianness (word byte order) used when reading numbers.

		If `true`, big-endian is used, otherwise `little-endian` is used.
	**/
	public var bigEndian(default,set) : Bool;

	#if cs
	private var helper:BytesData;
	#elseif java
	private var helper:java.nio.ByteBuffer;
	#end

	/**
		Read and return one byte.
	**/
	public function readByte() : Int {
	#if cpp
		throw "Not implemented";
	#else
		return throw "Not implemented";
	#end
	}

	/**
		Read `len` bytes and write them into `s` to the position specified by `pos`.

		Returns the actual length of read data that can be smaller than `len`.

		See `readFullBytes` that tries to read the exact amount of specified bytes.
	**/
	public function readBytes( s : Bytes, pos : Int, len : Int ) : Int {
		var k = len;
		var b = #if (js || hl) @:privateAccess s.b #else s.getData() #end;
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		try {
			while( k > 0 ) {
				#if neko
					untyped __dollar__sset(b,pos,readByte());
				#elseif php
					b.set(pos, readByte());
				#elseif cpp
					b[pos] = untyped readByte();
				#else
					b[pos] = cast readByte();
				#end
				pos++;
				k--;
			}
		} catch (eof: haxe.io.Eof){}
		return len-k;
	}

	/**
		Close the input source.

		Behaviour while reading after calling this method is unspecified.
	**/
	public function close() : Void {
	}

	function set_bigEndian( b : Bool ) : Bool {
		bigEndian = b;
		return b;
	}

	/* ------------------ API ------------------ */

	/**
		Read and return all available data.

		The `bufsize` optional argument specifies the size of chunks by
		which data is read. Its default value is target-specific.
	**/
	public function readAll( ?bufsize : Int ) : Bytes {
		if( bufsize == null )
		#if php
			bufsize = 8192; // default value for PHP and max under certain circumstances
		#else
			bufsize = (1 << 14); // 16 Ko
		#end

		var buf = Bytes.alloc(bufsize);
		var total = new haxe.io.BytesBuffer();
		try {
			while( true ) {
				var len = readBytes(buf,0,bufsize);
				if( len == 0 )
					throw Error.Blocked;
				total.addBytes(buf,0,len);
			}
		} catch( e : Eof ) { }
		return total.getBytes();
	}

	/**
		Read `len` bytes and write them into `s` to the position specified by `pos`.

		Unlike `readBytes`, this method tries to read the exact `len` amount of bytes.
	**/
	public function readFullBytes( s : Bytes, pos : Int, len : Int ) : Void {
		while( len > 0 ) {
			var k = readBytes(s,pos,len);
			if (k == 0)
				throw Error.Blocked;
			pos += k;
			len -= k;
		}
	}

	/**
		Read and return `nbytes` bytes.
	**/
	public function read( nbytes : Int ) : Bytes {
		var s = Bytes.alloc(nbytes);
		var p = 0;
		while( nbytes > 0 ) {
			var k = readBytes(s,p,nbytes);
			if( k == 0 ) throw Error.Blocked;
			p += k;
			nbytes -= k;
		}
		return s;
	}

	/**
		Read a string until a character code specified by `end` is occurred.

		The final character is not included in the resulting string.
	**/
	public function readUntil( end : Int ) : String {
		var buf = new BytesBuffer();
		var last : Int;
		while( (last = readByte()) != end )
			buf.addByte( last );
		return buf.getBytes().toString();
	}

	/**
		Read a line of text separated by CR and/or LF bytes.

		The CR/LF characters are not included in the resulting string.
	**/
	public function readLine() : String {
		var buf = new BytesBuffer();
		var last : Int;
		var s;
		try {
			while( (last = readByte()) != 10 )
				buf.addByte( last );
			s = buf.getBytes().toString();
			if( s.charCodeAt(s.length-1) == 13 ) s = s.substr(0,-1);
		} catch( e : Eof ) {
			s = buf.getBytes().toString();
			if( s.length == 0 )
				#if neko neko.Lib.rethrow #else throw #end (e);
		}
		return s;
	}

	/**
		Read a 32-bit floating point number.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readFloat() : Float {
		return FPHelper.i32ToFloat(readInt32());
	}

	/**
		Read a 64-bit double-precision floating point number.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readDouble() : Float {
		var i1 = readInt32();
		var i2 = readInt32();
		return bigEndian ? FPHelper.i64ToDouble(i2,i1) : FPHelper.i64ToDouble(i1,i2);
	}

	/**
		Read a 8-bit signed integer.
	**/
	public function readInt8() : Int {
		var n = readByte();
		if( n >= 128 )
			return n - 256;
		return n;
	}

	/**
		Read a 16-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readInt16() : Int {
		var ch1 = readByte();
		var ch2 = readByte();
		var n = bigEndian ? ch2 | (ch1 << 8) : ch1 | (ch2 << 8);
		if( n & 0x8000 != 0 )
			return n - 0x10000;
		return n;
	}

	/**
		Read a 16-bit unsigned integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readUInt16() : Int {
		var ch1 = readByte();
		var ch2 = readByte();
		return bigEndian ? ch2 | (ch1 << 8) : ch1 | (ch2 << 8);
	}

	/**
		Read a 24-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readInt24() : Int {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var n = bigEndian ? ch3 | (ch2 << 8) | (ch1 << 16) : ch1 | (ch2 << 8) | (ch3 << 16);
		if( n & 0x800000 != 0 )
			return n - 0x1000000;
		return n;
	}

	/**
		Read a 24-bit unsigned integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readUInt24() : Int {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		return bigEndian ? ch3 | (ch2 << 8) | (ch1 << 16) : ch1 | (ch2 << 8) | (ch3 << 16);
	}

	/**
		Read a 32-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function readInt32() : Int {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var ch4 = readByte();
#if (php || python)
		// php will overflow integers.  Convert them back to signed 32-bit ints.
		var n = bigEndian ? ch4 | (ch3 << 8) | (ch2 << 16) | (ch1 << 24) : ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
		if (n & 0x80000000 != 0)
			return ( n | 0x80000000);
		else return n;
#elseif lua
		var n = bigEndian ? ch4 | (ch3 << 8) | (ch2 << 16) | (ch1 << 24) : ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
		return lua.Boot.clamp(n);
#else
		return bigEndian ? ch4 | (ch3 << 8) | (ch2 << 16) | (ch1 << 24) : ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
#end
	}

	/**
		Read and `len` bytes as a string.
	**/
	public function readString( len : Int ) : String {
		var b = Bytes.alloc(len);
		readFullBytes(b,0,len);
		#if neko
		return neko.Lib.stringReference(b);
		#else
		return b.toString();
		#end
	}

#if neko
	static var _float_of_bytes = neko.Lib.load("std","float_of_bytes",2);
	static var _double_of_bytes = neko.Lib.load("std","double_of_bytes",2);
	static function __init__() untyped {
		Input.prototype.bigEndian = false;
	}
#end

#if (flash || js || python)
	function getDoubleSig(bytes:Array<Int>)
	{
		return (((bytes[1]&0xF) << 16) | (bytes[2] << 8) | bytes[3] ) * 4294967296. +
			(bytes[4] >> 7) * 2147483648 +
			(((bytes[4]&0x7F) << 24) | (bytes[5] << 16) | (bytes[6] << 8) | bytes[7]);
	}
#end

}

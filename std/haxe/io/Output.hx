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
	An Output is an abstract write. A specific output implementation will only
	have to override the `writeByte` and maybe the `write`, `flush` and `close`
	methods. See `File.write` and `String.write` for two ways of creating an
	Output.
**/
class Output {

	/**
		Endianness (word byte order) used when writing numbers.

		If `true`, big-endian is used, otherwise `little-endian` is used.
	**/
	public var bigEndian(default, set) : Bool;

	#if java
	private var helper:java.nio.ByteBuffer;
	#end

	/**
		Write one byte.
	**/
	public function writeByte( c : Int ) : Void {
		throw "Not implemented";
	}

	/**
		Write `len` bytes from `s` starting by position specified by `pos`.

		Returns the actual length of written data that can differ from `len`.

		See `writeFullBytes` that tries to write the exact amount of specified bytes.
	**/
	public function writeBytes( s : Bytes, pos : Int, len : Int ) : Int {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		#end
		var b = #if js @:privateAccess s.b #else s.getData() #end;
		var k = len;
		while( k > 0 ) {
			#if neko
				writeByte(untyped __dollar__sget(b,pos));
			#elseif php
				writeByte(b.get(pos));
			#elseif cpp
				writeByte(untyped b[pos]);
			#elseif hl
				writeByte(b[pos]);
			#else
				writeByte(untyped b[pos]);
			#end
			pos++;
			k--;
		}
		return len;
	}

	/**
		Flush any buffered data.
	**/
	public function flush() {
	}

	/**
		Close the output.

		Behaviour while writing after calling this method is unspecified.
	**/
	public function close() {
	}

	function set_bigEndian( b ) {
		bigEndian = b;
		return b;
	}

	/* ------------------ API ------------------ */

	/**
		Write all bytes stored in `s`.
	**/
	public function write( s : Bytes ) : Void {
		var l = s.length;
		var p = 0;
		while( l > 0 ) {
			var k = writeBytes(s,p,l);
			if( k == 0 ) throw Error.Blocked;
			p += k;
			l -= k;
		}
	}

	/**
		Write `len` bytes from `s` starting by position specified by `pos`.

		Unlike `writeBytes`, this method tries to write the exact `len` amount of bytes.
	**/
	public function writeFullBytes( s : Bytes, pos : Int, len : Int ) {
		while( len > 0 ) {
			var k = writeBytes(s,pos,len);
			pos += k;
			len -= k;
		}
	}

	/**
		Write `x` as 32-bit floating point number.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeFloat( x : Float ) {
		writeInt32(FPHelper.floatToI32(x));
	}

	/**
		Write `x` as 64-bit double-precision floating point number.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeDouble( x : Float ) {
		var i64 = FPHelper.doubleToI64(x);
		if( bigEndian ) {
			writeInt32(i64.high);
			writeInt32(i64.low);
		} else {
			writeInt32(i64.low);
			writeInt32(i64.high);
		}
	}

	/**
		Write `x` as 8-bit signed integer.
	**/
	public function writeInt8( x : Int ) {
		if( x < -0x80 || x >= 0x80 )
			throw Error.Overflow;
		writeByte(x & 0xFF);
	}

	/**
		Write `x` as 16-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeInt16( x : Int ) {
		if( x < -0x8000 || x >= 0x8000 ) throw Error.Overflow;
		writeUInt16(x & 0xFFFF);
	}

	/**
		Write `x` as 16-bit unsigned integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeUInt16( x : Int ) {
		if( x < 0 || x >= 0x10000 ) throw Error.Overflow;
		if( bigEndian ) {
			writeByte(x >> 8);
			writeByte(x & 0xFF);
		} else {
			writeByte(x & 0xFF);
			writeByte(x >> 8);
		}
	}

	/**
		Write `x` as 24-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeInt24( x : Int ) {
		if( x < -0x800000 || x >= 0x800000 ) throw Error.Overflow;
		writeUInt24(x & 0xFFFFFF);
	}

	/**
		Write `x` as 24-bit unsigned integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeUInt24( x : Int ) {
		if( x < 0 || x >= 0x1000000 ) throw Error.Overflow;
		if( bigEndian ) {
			writeByte(x >> 16);
			writeByte((x >> 8) & 0xFF);
			writeByte(x & 0xFF);
		} else {
			writeByte(x & 0xFF);
			writeByte((x >> 8) & 0xFF);
			writeByte(x >> 16);
		}
	}

	/**
		Write `x` as 32-bit signed integer.

		Endianness is specified by the `bigEndian` property.
	**/
	public function writeInt32( x : Int ) {
		if( bigEndian ) {
			writeByte( x >>> 24 );
			writeByte( (x >> 16) & 0xFF );
			writeByte( (x >> 8) & 0xFF );
			writeByte( x & 0xFF );
		} else {
			writeByte( x & 0xFF );
			writeByte( (x >> 8) & 0xFF );
			writeByte( (x >> 16) & 0xFF );
			writeByte( x >>> 24 );
		}
	}

	/**
		Inform that we are about to write at least `nbytes` bytes.

		The underlying implementation can allocate proper working space depending
		on this information, or simply ignore it. This is not a mandatory call
		but a tip and is only used in some specific cases.
	**/
	public function prepare( nbytes : Int ) {
	}

	/**
		Read all available data from `i` and write it.

		The `bufsize` optional argument specifies the size of chunks by
		which data is read and written. Its default value is 4096.
	**/
	public function writeInput( i : Input, ?bufsize : Int ) {
		if( bufsize == null )
			bufsize = 4096;
		var buf = Bytes.alloc(bufsize);
		try {
			while( true ) {
				var len = i.readBytes(buf,0,bufsize);
				if( len == 0 )
					throw Error.Blocked;
				var p = 0;
				while( len > 0 ) {
					var k = writeBytes(buf,p,len);
					if( k == 0 )
						throw Error.Blocked;
					p += k;
					len -= k;
				}
			}
		} catch( e : Eof ) {
		}
	}

	/**
		Write `s` string.
	**/
	public function writeString( s : String ) {
		#if neko
		var b = untyped new Bytes(s.length,s.__s);
		#else
		var b = Bytes.ofString(s);
		#end
		writeFullBytes(b,0,b.length);
	}

#if neko
	static function __init__() untyped {
		Output.prototype.bigEndian = false;
	}
#end

}

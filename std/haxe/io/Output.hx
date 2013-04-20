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

/**
	An Output is an abstract write. A specific output implementation will only
	have to override the [writeChar] and maybe the [write], [flush] and [close]
	methods. See [File.write] and [String.write] for two ways of creating an
	Output.
**/
class Output {
	private static var LN2 = Math.log(2);
	public var bigEndian(default, set) : Bool;

	#if java
	private var helper:java.nio.ByteBuffer;
	#end

	public function writeByte( c : Int ) : Void {
		throw "Not implemented";
	}

	public function writeBytes( s : Bytes, pos : Int, len : Int ) : Int {
		var k = len;
		var b = s.getData();
		#if !neko
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		#end
		while( k > 0 ) {
			#if neko
				writeByte(untyped __dollar__sget(b,pos));
			#elseif php
				writeByte(untyped __call__("ord", b[pos]));
			#elseif cpp
				writeByte(untyped b[pos]);
			#else
				writeByte(untyped b[pos]);
			#end
			pos++;
			k--;
		}
		return len;
	}

	public function flush() {
	}

	public function close() {
	}

	function set_bigEndian( b ) {
		bigEndian = b;
		return b;
	}

	/* ------------------ API ------------------ */

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

	public function writeFullBytes( s : Bytes, pos : Int, len : Int ) {
		while( len > 0 ) {
			var k = writeBytes(s,pos,len);
			pos += k;
			len -= k;
		}
	}

	public function writeFloat( x : Float ) {
		#if neko
		write(untyped new Bytes(4,_float_bytes(x,bigEndian)));
		#elseif cpp
		write(Bytes.ofData(_float_bytes(x,bigEndian)));
		#elseif php
		write(untyped Bytes.ofString(__call__('pack', 'f', x)));
		#elseif cs
		var bytes = cs.system.BitConverter.GetBytes(cast(x, Single));
		if (bigEndian == cs.system.BitConverter.IsLittleEndian)
		{
			writeByte(bytes[3]);
			writeByte(bytes[2]);
			writeByte(bytes[1]);
			writeByte(bytes[0]);
		} else {
			writeByte(bytes[0]);
			writeByte(bytes[1]);
			writeByte(bytes[2]);
			writeByte(bytes[3]);
		}
		#elseif java
		if (helper == null) helper = java.nio.ByteBuffer.allocateDirect(8);
		var helper = helper;
		helper.order(bigEndian ? java.nio.ByteOrder.BIG_ENDIAN : java.nio.ByteOrder.LITTLE_ENDIAN);

		helper.putFloat(0, x);
		writeByte(untyped helper.get(0));
		writeByte(untyped helper.get(1));
		writeByte(untyped helper.get(2));
		writeByte(untyped helper.get(3));
		#else
		if (x == 0.0)
		{
			writeByte(0); writeByte(0); writeByte(0); writeByte(0);
			return;
		}
		var exp = Math.floor(Math.log(Math.abs(x)) / LN2);
		var sig = (Math.floor(Math.abs(x) / Math.pow(2, exp) * (2 << 22)) & 0x7FFFFF);
		var b1 = (exp + 0x7F) >> 1 | (exp>0 ? ((x<0) ? 1<<7 : 1<<6) : ((x<0) ? 1<<7 : 0)),
			b2 = (exp + 0x7F) << 7 & 0xFF | (sig >> 16 & 0x7F),
			b3 = (sig >> 8) & 0xFF,
			b4 = sig & 0xFF;
		if (bigEndian)
		{
			writeByte(b4); writeByte(b3); writeByte(b2); writeByte(b1);
		}
		else
		{
			writeByte(b1); writeByte(b2); writeByte(b3); writeByte(b4);
		}
		#end
	}

	public function writeDouble( x : Float ) {
		#if neko
		write(untyped new Bytes(8,_double_bytes(x,bigEndian)));
		#elseif cpp
		write(Bytes.ofData(_double_bytes(x,bigEndian)));
		#elseif php
		write(untyped Bytes.ofString(__call__('pack', 'd', x)));
		#elseif cs
		var bytes = cs.system.BitConverter.GetBytes(x);
		if (bigEndian == cs.system.BitConverter.IsLittleEndian)
		{
			writeByte(bytes[7]);
			writeByte(bytes[6]);
			writeByte(bytes[5]);
			writeByte(bytes[4]);
			writeByte(bytes[3]);
			writeByte(bytes[2]);
			writeByte(bytes[1]);
			writeByte(bytes[0]);
		} else {
			writeByte(bytes[0]);
			writeByte(bytes[1]);
			writeByte(bytes[2]);
			writeByte(bytes[3]);
			writeByte(bytes[4]);
			writeByte(bytes[5]);
			writeByte(bytes[6]);
			writeByte(bytes[7]);
		}
		#elseif java
		if (helper == null) helper = java.nio.ByteBuffer.allocateDirect(8);
		var helper = helper;
		helper.order(bigEndian ? java.nio.ByteOrder.BIG_ENDIAN : java.nio.ByteOrder.LITTLE_ENDIAN);

		helper.putDouble(0, x);

		writeByte(untyped helper.get(0));
		writeByte(untyped helper.get(1));
		writeByte(untyped helper.get(2));
		writeByte(untyped helper.get(3));
		writeByte(untyped helper.get(4));
		writeByte(untyped helper.get(5));
		writeByte(untyped helper.get(6));
		writeByte(untyped helper.get(7));
		#else
		if (x == 0.0)
		{
			writeByte(0); writeByte(0); writeByte(0); writeByte(0);
			writeByte(0); writeByte(0); writeByte(0); writeByte(0);
			return;
		}

		var exp = Math.floor(Math.log(Math.abs(x)) / LN2);
		var sig : Int = Math.floor(Math.abs(x) / Math.pow(2, exp) * Math.pow(2, 52));
		var sig_h = (sig & cast 34359738367);
		var sig_l = Math.floor((sig / Math.pow(2,32)));
		var b1 = (exp + 0x3FF) >> 4 | (exp>0 ? ((x<0) ? 1<<7 : 1<<6) : ((x<0) ? 1<<7 : 0)),
			b2 = (exp + 0x3FF) << 4 & 0xFF | (sig_l >> 16 & 0xF),
			b3 = (sig_l >> 8) & 0xFF,
			b4 = sig_l & 0xFF,
			b5 = (sig_h >> 24) & 0xFF,
			b6 = (sig_h >> 16) & 0xFF,
			b7 = (sig_h >> 8) & 0xFF,
			b8 = sig_h & 0xFF;
		if (bigEndian)
		{
			writeByte(b8); writeByte(b7); writeByte(b6); writeByte(b5);
			writeByte(b4); writeByte(b3); writeByte(b2); writeByte(b1);
		}
		else
		{
			writeByte(b1); writeByte(b2); writeByte(b3); writeByte(b4);
			writeByte(b5); writeByte(b6); writeByte(b7); writeByte(b8);
		}
		#end
	}

	public function writeInt8( x : Int ) {
		if( x < -0x80 || x >= 0x80 )
			throw Error.Overflow;
		writeByte(x & 0xFF);
	}

	public function writeInt16( x : Int ) {
		if( x < -0x8000 || x >= 0x8000 ) throw Error.Overflow;
		writeUInt16(x & 0xFFFF);
	}

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

	public function writeInt24( x : Int ) {
		if( x < -0x800000 || x >= 0x800000 ) throw Error.Overflow;
		writeUInt24(x & 0xFFFFFF);
	}

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
		Inform that we are about to write at least a specified number of bytes.
		The underlying implementation can allocate proper working space depending
		on this information, or simply ignore it. This is not a mandatory call
		but a tip and is only used in some specific cases.
	**/
	public function prepare( nbytes : Int ) {
	}

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

	public function writeString( s : String ) {
		#if neko
		var b = untyped new Bytes(s.length,s.__s);
		#else
		var b = Bytes.ofString(s);
		#end
		writeFullBytes(b,0,b.length);
	}

#if neko
	static var _float_bytes = neko.Lib.load("std","float_bytes",2);
	static var _double_bytes = neko.Lib.load("std","double_bytes",2);
	static function __init__() untyped {
		Output.prototype.bigEndian = false;
	}
#elseif cpp
	static var _float_bytes = cpp.Lib.load("std","float_bytes",2);
	static var _double_bytes = cpp.Lib.load("std","double_bytes",2);
#end

}

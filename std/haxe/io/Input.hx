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
	An Input is an abstract reader. See other classes in the [haxe.io] package
	for several possible implementations.
**/
class Input {

	public var bigEndian(default,set) : Bool;
	#if cs
	private var helper:BytesData;
	#elseif java
	private var helper:java.nio.ByteBuffer;
	#end

	public function readByte() : Int {
	#if cpp
		throw "Not implemented";
		return 0;
	#else
		return throw "Not implemented";
	#end
	}

	public function readBytes( s : Bytes, pos : Int, len : Int ) : Int {
		var k = len;
		var b = s.getData();
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		while( k > 0 ) {
			#if neko
				untyped __dollar__sset(b,pos,readByte());
			#elseif php
				b[pos] = untyped __call__("chr", readByte());
			#elseif cpp
				b[pos] = untyped readByte();
			#else
				b[pos] = readByte();
			#end
			pos++;
			k--;
		}
		return len;
	}

	public function close() {
	}

	function set_bigEndian(b) {
		bigEndian = b;
		return b;
	}

	/* ------------------ API ------------------ */

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
		} catch( e : Eof ) {
		}
		return total.getBytes();
	}

	public function readFullBytes( s : Bytes, pos : Int, len : Int ) {
		while( len > 0 ) {
			var k = readBytes(s,pos,len);
			pos += k;
			len -= k;
		}
	}

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

	public function readUntil( end : Int ) : String {
		var buf = new StringBuf();
		var last : Int;
		while( (last = readByte()) != end )
			buf.addChar( last );
		return buf.toString();
	}

	public function readLine() : String {
		var buf = new StringBuf();
		var last : Int;
		var s;
		try {
			while( (last = readByte()) != 10 )
				buf.addChar( last );
			s = buf.toString();
			if( s.charCodeAt(s.length-1) == 13 ) s = s.substr(0,-1);
		} catch( e : Eof ) {
			s = buf.toString();
			if( s.length == 0 )
				#if neko neko.Lib.rethrow #else throw #end (e);
		}
		return s;
	}

	public function readFloat() : Float {
		#if neko
			return _float_of_bytes(untyped read(4).b,bigEndian);
		#elseif cpp
			return _float_of_bytes(read(4).getData(),bigEndian);
		#elseif php
			var a = untyped __call__('unpack', 'f', readString(4));
			return a[1];
		#elseif cs
			if (helper == null) helper = new cs.NativeArray(8);

			var helper = helper;
			if (bigEndian == !cs.system.BitConverter.IsLittleEndian)
			{
				helper[0] = readByte();
				helper[1] = readByte();
				helper[2] = readByte();
				helper[3] = readByte();
			} else {
				helper[3] = readByte();
				helper[2] = readByte();
				helper[1] = readByte();
				helper[0] = readByte();
			}

			return cs.system.BitConverter.ToSingle(helper, 0);
		#elseif java
			if (helper == null) helper = java.nio.ByteBuffer.allocateDirect(8);
			var helper = helper;
			helper.order(bigEndian ? java.nio.ByteOrder.BIG_ENDIAN : java.nio.ByteOrder.LITTLE_ENDIAN);

			helper.put(0, readByte());
			helper.put(1, readByte());
			helper.put(2, readByte());
			helper.put(3, readByte());

			return helper.getFloat(0);
		#else
			var bytes = [];
			bytes.push(readByte());
			bytes.push(readByte());
			bytes.push(readByte());
			bytes.push(readByte());
			if (bigEndian)
				bytes.reverse();
			var sign = 1 - ((bytes[0] >> 7) << 1);
			var exp = (((bytes[0] << 1) & 0xFF) | (bytes[1] >> 7)) - 127;
			var sig = ((bytes[1] & 0x7F) << 16) | (bytes[2] << 8) | bytes[3];
			if (sig == 0 && exp == -127)
				return 0.0;
			return sign*(1 + Math.pow(2, -23)*sig) * Math.pow(2, exp);
		#end
	}

	public function readDouble() : Float {
		#if neko
			return _double_of_bytes(untyped read(8).b,bigEndian);
		#elseif cpp
			return _double_of_bytes(read(8).getData(),bigEndian);
		#elseif php
			var a = untyped __call__('unpack', 'd', readString(8));
			return a[1];
		#elseif (flash || js)
		var bytes = [];
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		bytes.push(readByte());
		if (bigEndian)
			bytes.reverse();

		var sign = 1 - ((bytes[0] >> 7) << 1); // sign = bit 0
		var exp = (((bytes[0] << 4) & 0x7FF) | (bytes[1] >> 4)) - 1023; // exponent = bits 1..11
		var sig = getDoubleSig(bytes);
		if (sig == 0 && exp == -1023)
			return 0.0;
		return sign * (1.0 + Math.pow(2, -52) * sig) * Math.pow(2, exp);
		#elseif cs
		if (helper == null) helper = new cs.NativeArray(8);

		var helper = helper;
		if (bigEndian == !cs.system.BitConverter.IsLittleEndian)
		{
			helper[0] = readByte();
			helper[1] = readByte();
			helper[2] = readByte();
			helper[3] = readByte();
			helper[4] = readByte();
			helper[5] = readByte();
			helper[6] = readByte();
			helper[7] = readByte();
		} else {
			helper[7] = readByte();
			helper[6] = readByte();
			helper[5] = readByte();
			helper[4] = readByte();
			helper[3] = readByte();
			helper[2] = readByte();
			helper[1] = readByte();
			helper[0] = readByte();
		}

		return cs.system.BitConverter.ToDouble(helper, 0);
		#elseif java
		if (helper == null) helper = java.nio.ByteBuffer.allocateDirect(8);
		var helper = helper;
		helper.order(bigEndian ? java.nio.ByteOrder.BIG_ENDIAN : java.nio.ByteOrder.LITTLE_ENDIAN);

		helper.put(0, readByte());
		helper.put(1, readByte());
		helper.put(2, readByte());
		helper.put(3, readByte());
		helper.put(4, readByte());
		helper.put(5, readByte());
		helper.put(6, readByte());
		helper.put(7, readByte());

		return helper.getDouble(0);
		#else
		return throw "not implemented";
		#end
	}

	public function readInt8() {
		var n = readByte();
		if( n >= 128 )
			return n - 256;
		return n;
	}

	public function readInt16() {
		var ch1 = readByte();
		var ch2 = readByte();
		var n = bigEndian ? ch2 | (ch1 << 8) : ch1 | (ch2 << 8);
		if( n & 0x8000 != 0 )
			return n - 0x10000;
		return n;
	}

	public function readUInt16() {
		var ch1 = readByte();
		var ch2 = readByte();
		return bigEndian ? ch2 | (ch1 << 8) : ch1 | (ch2 << 8);
	}

	public function readInt24() {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var n = bigEndian ? ch3 | (ch2 << 8) | (ch1 << 16) : ch1 | (ch2 << 8) | (ch3 << 16);
		if( n & 0x800000 != 0 )
			return n - 0x1000000;
		return n;
	}

	public function readUInt24() {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		return bigEndian ? ch3 | (ch2 << 8) | (ch1 << 16) : ch1 | (ch2 << 8) | (ch3 << 16);
	}

	#if haxe3

	public function readInt32() {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var ch4 = readByte();
		return bigEndian ? ch4 | (ch3 << 8) | (ch2 << 16) | (ch1 << 24) : ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
	}

	#else

	public function readInt31() {
		var ch1,ch2,ch3,ch4;
		if( bigEndian ) {
			ch4 = readByte();
			ch3 = readByte();
			ch2 = readByte();
			ch1 = readByte();
		} else {
			ch1 = readByte();
			ch2 = readByte();
			ch3 = readByte();
			ch4 = readByte();
		}
		if( ((ch4 & 128) == 0) != ((ch4 & 64) == 0) ) throw Error.Overflow;
		return ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
	}

	public function readUInt30() {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var ch4 = readByte();
		if( (bigEndian?ch1:ch4) >= 64 ) throw Error.Overflow;
		return bigEndian ? ch4 | (ch3 << 8) | (ch2 << 16) | (ch1 << 24) : ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
	}

	public function readInt32() {
		var ch1 = readByte();
		var ch2 = readByte();
		var ch3 = readByte();
		var ch4 = readByte();
#if php
		var i = bigEndian ? ((ch1 << 8 | ch2) << 16 | (ch3 << 8 | ch4)) : ((ch4 << 8 | ch3) << 16 | (ch2 << 8 | ch1));
		if (i > 0x7FFFFFFF)
			untyped __php__("$i -= 0x100000000");
		return haxe.Int32.ofInt(i);
#else
		return bigEndian ? haxe.Int32.make((ch1 << 8) | ch2, (ch3 << 8) | ch4) : haxe.Int32.make((ch4 << 8) | ch3, (ch2 << 8) | ch1);
#end
	}

	#end

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
#elseif cpp
	static var _float_of_bytes = cpp.Lib.load("std","float_of_bytes",2);
	static var _double_of_bytes = cpp.Lib.load("std","double_of_bytes",2);
#end

#if (flash || js)
	function getDoubleSig(bytes:Array<Int>)
    {
        return (((bytes[1]&0xF) << 16) | (bytes[2] << 8) | bytes[3] ) * 4294967296. +
            (bytes[4] >> 7) * 2147483648 +
            (((bytes[4]&0x7F) << 24) | (bytes[5] << 16) | (bytes[6] << 8) | bytes[7]);
    }
#end

}
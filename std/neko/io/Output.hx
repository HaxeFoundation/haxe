/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package neko.io;

/**
	An Output is an abstract write. A specific output implementation will only
	have to override the [writeChar] and maybe the [write], [flush] and [close]
	methods. See [File.write] and [String.write] for two ways of creating an
	Output.
**/
class Output {

	public function writeChar( c : Int ) : Void {
		throw "Not implemented";
	}

	public function writeBytes( s : String, p : Int, len : Int ) : Int {
		var k = len;
		while( k > 0 ) {
			writeChar(untyped __dollar__sget(s.__s,p));
			p += 1;
			k -= 1;
		}
		return len;
	}

	public function flush() {
	}

	public function close() {
		writeBytes = function(_,_,_) { return throw Error.Closed; };
		writeChar = function(_) { throw Error.Closed; };
		flush = function() { throw Error.Closed; };
		close = function() { };
	}

	/* ------------------ API ------------------ */

	public function write( s : String ) : Void {
		var l = s.length;
		var p = 0;
		while( l > 0 ) {
			var k = writeBytes(s,p,l);
			if( k == 0 ) throw Error.Blocked;
			p += k;
			l -= k;
		}
	}

	public function writeFullBytes( s : String, pos : Int, len : Int ) {
		while( len > 0 ) {
			var k = writeBytes(s,pos,len);
			pos += k;
			len -= k;
		}
	}

	public function writeFloat( c : Float ) {
		write(new String(_float_bytes(c,false)));
	}

	public function writeFloatB( c : Float ) {
		write(new String(_float_bytes(c,true)));
	}

	public function writeDouble( c : Float ) {
		write(new String(_double_bytes(c,false)));
	}

	public function writeDoubleB( c : Float ) {
		write(new String(_double_bytes(c,true)));
	}

	public function writeInt8( c : Int ) {
		if( c < -0x80 || c > 0x7F )
			throw Error.Overflow;
		writeChar(c & 0xFF);
	}

	public function writeInt16( x : Int ) {
		if( x < -0x8000 || x > 0x7FFF ) throw Error.Overflow;
		if( x < 0 )
			writeUInt16(0x10000 + x);
		else
			writeUInt16(x);
	}

	public function writeUInt16( x : Int ) {
		if( x < 0 || x > 0xFFFF ) throw Error.Overflow;
		writeChar(x & 0xFF);
		writeChar(x >> 8);
	}

	public function writeUInt16B( x : Int ) {
		if( x < 0 || x > 0xFFFF ) throw Error.Overflow;
		writeChar(x >> 8);
		writeChar(x & 0xFF);
	}

	public function writeInt24( x : Int ) {
		if( x < -0x800000 || x > 0x7FFFFF ) throw Error.Overflow;
		if( x < 0 )
			writeUInt24(0x1000000 + x);
		else
			writeUInt24(x);
	}
	
	public function writeUInt24( x : Int ) {
		if( x < 0 || x > 0xFFFFFF ) throw Error.Overflow;
		writeChar(x & 0xFF);
		writeChar((x >> 8) & 0xFF);
		writeChar(x >> 16);
	}
	
	public function writeUInt24B( x : Int ) {
		if( x < 0 || x > 0xFFFFFF ) throw Error.Overflow;
		writeChar(x >> 16);
		writeChar((x >> 8) & 0xFF);
		writeChar(x & 0xFF);
	}

	public function writeInt32( x : Int ) {
		writeChar(x & 0xFF);
		writeChar((x >> 8) & 0xFF);
		writeChar((x >> 16) & 0xFF);
		writeChar(x >>> 24);
	}

	public function writeUInt32( x : Int ) {
		if( x < 0 ) throw Error.Overflow;
		writeInt32(x);
	}
	
	public function writeUInt32B( x : Int ) {
		if( x < 0 ) throw Error.Overflow;
		writeChar(x >>> 24);
		writeChar((x >> 16) & 0xFF);
		writeChar((x >> 8) & 0xFF);
		writeChar(x & 0xFF);
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
		var buf = neko.Lib.makeString(bufsize);
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

	static var _float_bytes = neko.Lib.load("std","float_bytes",2);
	static var _double_bytes = neko.Lib.load("std","double_bytes",2);

}

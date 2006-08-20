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
	An Input is an abstract reader. A specific input implementation will only
	have to override the [readChar] and maybe [read] and [close] methods. See
	[File] and [StringInput] for two ways of creating an Input.
**/
class Input {

	public function readChar() : Int {
		return throw "Not implemented";
	}

	public function read( s : String, p : Int, len : Int ) : Int {
		var k = len;
		while( k > 0 ) {
			var c = readChar();
			untyped __dollar__sset(s.__s,p,c);
			p += 1;
			k -= 1;
		}
		return len;
	}

	public function close() {
		read = function(_,_,_) { return throw Error.Closed; };
		readChar = function() { return throw Error.Closed; };
		close = function() { };
	}


	/* ------------------ API ------------------ */

	public function readAll( ?bufsize : Int ) : String {
		if( bufsize == null )
			bufsize = (1 << 14); // 16 Ko
		var buf = neko.Lib.makeString(bufsize);
		var total = new StringBuf();
		try {
			while( true ) {
				var len = read(buf,0,bufsize);
				if( len == 0 )
					throw Error.Blocked;
				total.addSub(buf,0,len);
			}
		} catch( e : Error ) {
			if( e != Error.Eof )
				neko.Lib.rethrow(e);
		}
		return total.toString();
	}

	public function readBytes( nbytes : Int ) : String {
		var s = neko.Lib.makeString(nbytes);
		var p = 0;
		while( nbytes > 0 ) {
			var k = read(s,p,nbytes);
			if( k == 0 ) throw Error.Blocked;
			p += k;
			nbytes -= k;
		}
		return s;
	}

	public function readUntil( end : Int ) : String {
		var buf = new StringBuf();
		var last : Int;
		while( (last = readChar()) != end )
			buf.addChar( last );
		return buf.toString();
	}

	public function readLine() : String {
		var s = readUntil( 10 );
		if( s.substr(-1,1) == "\r" ) return s.substr(0,-1);
		return s;
	}

	public function skip( nbytes : Int ) {
		readBytes(nbytes);
	}

	public function readInt32() {
		var ch1 = readChar();
		var ch2 = readChar();
		var ch3 = readChar();
		var ch4 = readChar();
		if( (ch4 & 128) != 0 ) {
			if( ch4 & 64 == 0 ) throw Error.Overflow;
			return ch1 | (ch2 << 8) | (ch3 << 16) | ((ch4 & 127) << 24);
		} else {
			if( ch4 & 64 != 0 ) throw Error.Overflow;
			return ch1 | (ch2 << 8) | (ch3 << 16) | (ch4 << 24);
		}
	}

	public function readUInt16() {
		var ch1 = readChar();
		var ch2 = readChar();
		return ch1 | (ch2 << 8);
	}

	public function readInt16() {
		var ch1 = readChar();
		var ch2 = readChar();
		var n = ch1 | (ch2 << 8);
		if( ch2 & 128 != 0 )
			return n - 65536;
		return n;
	}

}

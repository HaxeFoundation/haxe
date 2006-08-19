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

	public function write( s : String, p : Int, len : Int ) : Int {
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
		write = function(_,_,_) { return throw Error.Closed; };
		writeChar = function(_) { throw Error.Closed; };
		flush = close = function() { throw Error.Closed; };
	}

	/* ------------------ API ------------------ */

	public function writeBytes( s : String ) : Void {
		var l = s.length;
		var p = 0;
		while( l > 0 ) {
			var k = write(s,p,l);
			if( k == 0 ) throw Error.Blocked;
			p += k;
			l -= k;
		}
	}

	public function writeInt32( x : Int ) {
		writeChar(x & 0xFF);
		writeChar((x >> 8) & 0xFF);
		writeChar((x >> 16) & 0xFF);
		writeChar(x >>> 24);
	}

	public function writeUInt16( x : Int ) {
		if( x < 0 || x > 0xFFFF ) throw Error.Overflow;
		writeChar(x & 0xFF);
		writeChar(x >> 8);
	}

	public function writeInt16( x : Int ) {
		if( x < -0x7FFF || x > 0x7FFF ) throw Error.Overflow;
		if( x < 0 )
			writeUInt16(65536 + x);
		else
			writeUInt16(x);
	}

	/**
		Inform that we are about to write at least a specified number of bytes.
		The underlying implementation can allocate proper working space depending
		on this information, or simply ignore it. This is not a mandatory call
		but a tip and is only used in some specific cases.
	**/
	public function prepare( nbytes : Int ) {
	}

}

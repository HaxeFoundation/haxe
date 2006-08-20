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

class StringInput extends Input {

	var s : String;
	var pos : Int;
	var len : Int;

	public function new( s : String, ?pos : Int, ?len : Int ) {
		this.s = s;
		this.pos = if( pos == null ) 0 else pos;
		this.len = if( len == null ) s.length else len;
		if( this.pos < 0 || this.len < 0 )
			throw "Invalid parameter";
	}

	public override function readChar() {
		if( this.len == 0 )
			throw Error.Eof;
		var c = untyped __dollar__sget(s.__s,pos);
		pos += 1;
		len -= 1;
		return c;
	}

	public override function read( buf : String, bpos, blen ) : Int {
		if( len == 0 )
			throw Error.Eof;
		if( len <= blen )
			blen = len;
		untyped __dollar__sblit(buf.__s,bpos,s.__s,pos,blen);
		pos += blen;
		len -= blen;
		return blen;
	}

	public override function skip( nbytes : Int ) {
		pos += nbytes;
		len -= nbytes;
		if( len < 0 ) {
			len = 0;
			throw Error.Eof;
		}
	}

}

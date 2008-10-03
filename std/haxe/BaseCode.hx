/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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
package haxe;

class BaseCode {

	var base : haxe.io.Bytes;
	var nbits : Int;
	var tbl : Array<Int>;

	public function new( base : haxe.io.Bytes ) {
		var len = base.length;
		var nbits = 1;
		while( len > 1 << nbits )
			nbits++;
		if( nbits > 8 || len != 1 << nbits )
			throw "BaseCode : base length must be a power of two.";
		this.base = base;
		this.nbits = nbits;
	}

	public function encodeBytes( b : haxe.io.Bytes ) : haxe.io.Bytes {
		#if neko
		return haxe.io.Bytes.ofData( base_encode(b.getData(),base.getData()) );
		#else
		var nbits = this.nbits;
		var base = this.base;
		var size = Std.int(b.length * 8 / nbits);
		var out = haxe.io.Bytes.alloc(size + (((b.length * 8) % nbits == 0) ? 0 : 1) );
		var buf = 0;
		var curbits = 0;
		var mask = (1 << nbits) - 1;
		var pin = 0;
		var pout = 0;
		while( pout < size ) {
			while( curbits < nbits ) {
				curbits += 8;
				buf <<= 8;
				buf |= b.get(pin++);
			}
			curbits -= nbits;
			out.set(pout++,base.get((buf >> curbits) & mask));
		}
		if( curbits > 0 )
			out.set(pout++,base.get((buf << (nbits - curbits)) & mask));
		return out;
		#end
	}

	function initTable() {
		var tbl = new Array();
		for( i in 0...256 )
			tbl[i] = -1;
		for( i in 0...base.length )
			tbl[base.get(i)] = i;
		this.tbl = tbl;
	}

	public function decodeBytes( b : haxe.io.Bytes ) : haxe.io.Bytes {
		#if neko
		return haxe.io.Bytes.ofData( base_decode(b.getData(),base.getData()) );
		#else
		var nbits = this.nbits;
		var base = this.base;
		if( this.tbl == null ) initTable();
		var tbl = this.tbl;
		var size = (b.length * nbits) >> 3;
		var out = haxe.io.Bytes.alloc(size);
		var buf = 0;
		var curbits = 0;
		var pin = 0;
		var pout = 0;
		while( pout < size ) {
			while( curbits < 8 ) {
				curbits += nbits;
				buf <<= nbits;
				var i = tbl[b.get(pin++)];
				if( i == -1 )
					throw "BaseCode : invalid encoded char";
				buf |= i;
			}
			curbits -= 8;
			out.set(pout++,(buf >> curbits) & 0xFF);
		}
		return out;
		#end
	}

	public function encodeString( s : String ) {
		#if neko
		return neko.NativeString.toString( base_encode(neko.NativeString.ofString(s),base.getData()) );
		#else
		return encodeBytes(haxe.io.Bytes.ofString(s)).toString();
		#end
	}

	public function decodeString( s : String ) {
		#if neko
		return neko.NativeString.toString( base_decode(neko.NativeString.ofString(s),base.getData()) );
		#else
		return decodeBytes(haxe.io.Bytes.ofString(s)).toString();
		#end
	}

	public static function encode( s : String, base : String ) {
		var b = new BaseCode(haxe.io.Bytes.ofString(base));
		return b.encodeString(s);
	}

	public static function decode( s : String, base : String ) {
		var b = new BaseCode(haxe.io.Bytes.ofString(base));
		return b.decodeString(s);
	}

	#if neko
	private static var base_encode = neko.Lib.load("std","base_encode",2);
	private static var base_decode = neko.Lib.load("std","base_decode",2);
	#end
}

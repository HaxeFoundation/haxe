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

import php.*;

class BytesBuffer {
	var b : String;

	/** The length of the buffer in bytes. **/
	public var length(get,never) : Int;

	public function new() {
		b = "";
	}

	public inline function addByte( byte : Int ) {
		Syntax.binop(b, '.=', Global.chr(byte));
	}

	public inline function add( src : Bytes ) {
		Syntax.binop(b, '.=', src.getData().toNativeString());
	}

	public inline function addString( v : String ) {
		Syntax.binop(b, '.=', v);
	}

	public function addInt32( v : Int ) {
		addByte(v&0xFF);
		addByte((v>>8)&0xFF);
		addByte((v>>16)&0xFF);
		addByte(v>>>24);
	}

	public function addInt64( v : haxe.Int64 ) {
		addInt32(v.low);
		addInt32(v.high);
	}

	public inline function addFloat( v : Float ) {
		addInt32(FPHelper.floatToI32(v));
	}

	public inline function addDouble( v : Float ) {
		addInt64(FPHelper.doubleToI64(v));
	}

	public inline function addBytes( src : Bytes, pos : Int, len : Int ) {
		if( pos < 0 || len < 0 || pos + len > src.length ) {
			throw Error.OutsideBounds;
		} else {
			Syntax.binop(b, '.=', src.getData().sub(pos, len).toString());
		}
	}

	/**
		Returns either a copy or a reference of the current bytes.
		Once called, the buffer can no longer be used.
	**/
	public function getBytes() : Bytes untyped {
		var bytes = new Bytes(b.length, b);
		b = null;
		return bytes;
	}

	inline function get_length() : Int {
		return b.length;
	}
}

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
package hl;

@:coreType abstract BytesAccess<T> from Bytes to Bytes {

	public var sizeBits(get, never) : Int;
	public var nullValue(get, never) : T;


	@:extern inline function get_sizeBits() {
		return untyped $bytes_sizebits(this);
	}

	@:extern inline function get_nullValue() {
		return untyped $bytes_nullvalue(this);
	}

	@:extern public inline function blit( pos : Int, src : BytesAccess<T>, srcPos : Int, len : Int ) : Void {
		(this:Bytes).blit(pos << sizeBits, src, srcPos << sizeBits, len << sizeBits);
	}

	@:extern @:arrayAccess public inline function get( pos : Int ) : T {
		return untyped $bget(this,pos);
	}

	@:extern @:arrayAccess public inline function set( pos : Int, value : T ) : T {
		untyped $bset(this,pos,value);
		return value;
	}

}
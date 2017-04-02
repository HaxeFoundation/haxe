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

typedef UInt16ArrayData = js.html.Uint16Array;

@:coreApi
abstract UInt16Array(UInt16ArrayData) {

	public static inline var BYTES_PER_ELEMENT = 2;
	public var length(get,never) : Int;
	public var view(get,never) : ArrayBufferView;

	public inline function new( elements : Int ) {
		this = new UInt16ArrayData(elements);
	}

	inline function get_length() : Int {
		return this.length;
	}

	public inline function get_view() : ArrayBufferView {
		return ArrayBufferView.fromData(this);
	}

	@:arrayAccess public inline function get( index : Int ) : Int {
		return this[index];
	}

	@:arrayAccess public inline function set( index : Int, value : Int ) : Int {
		return this[index] = value & 0xFFFF; // &0xFFFF necessary for html compat
	}

	public inline function sub( begin : Int, ?length : Int ) : UInt16Array {
		return fromData(this.subarray(begin, length == null ? this.length : begin+length));
	}

	public inline function subarray( ?begin : Int, ?end : Int ) : UInt16Array {
		return fromData(this.subarray(begin, end));
	}

	public inline function getData() : UInt16ArrayData {
		return this;
	}

	public static inline function fromData( d : UInt16ArrayData ) : UInt16Array {
		return cast d;
	}

	public static function fromArray( a : Array<Int>, pos : Int = 0, ?length : Int ) : UInt16Array {
		if( length == null ) length = a.length - pos;
		if( pos < 0 || length < 0 || pos + length > a.length ) throw Error.OutsideBounds;
		if( pos == 0 && length == a.length )
			return fromData(new UInt16ArrayData(a));
		var i = new UInt16Array(a.length);
		for( idx in 0...length )
			i[idx] = a[idx + pos];
		return i;
	}

	public static function fromBytes( bytes : haxe.io.Bytes, bytePos : Int = 0, ?length : Int ) : UInt16Array {
		if( length == null ) length = (bytes.length - bytePos) >> 1;
		return fromData(new UInt16ArrayData(bytes.getData(), bytePos, length));
	}

}


/*
 * Copyright (C)2005-2015 Haxe Foundation
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

typedef Uint8ArrayData = #if js js.html.Uint8Array #else ArrayBufferView.ArrayBufferViewData #end

abstract Uint8Array(Uint8ArrayData) {

	public static inline var BYTES_PER_ELEMENT = 1;
	public var length(get,never) : Int;
	public var view(get,never) : ArrayBufferView;

	public inline function new( elements : Int ) {
		#if js
		this = new Uint8ArrayData(elements * BYTES_PER_ELEMENT);
		#else
		this = new ArrayBufferView(elements * BYTES_PER_ELEMENT).getData();
		#end
	}
	
	inline function get_length() {
		#if js
		return this.length;
		#else
		return this.byteLength;
		#end
	}

	public inline function get_view() : ArrayBufferView {
		return ArrayBufferView.fromData(this);
	}

	@:arrayAccess public inline function get( index : Int ) {
		#if js
		return this[index];
		#else
		return this.bytes.get(index + this.byteOffset);
		#end
	}
	
	@:arrayAccess public inline function set( index : Int, value : Int ) : Int {
		#if js
		return this[index] = value & 0xFF; // &0xFF necessary for html compat
		#else
		if( index >= 0 && index < length ) {
			this.bytes.set(index + this.byteOffset, value);
			return value;
		}
		return 0;
		#end
	}
	
	public inline function sub( begin : Int, ?length : Int ) {
		#if js
		return fromData(this.subarray(begin, length == null ? null : begin+length));
		#else
		return fromData(this.sub(begin,length));
		#end
	}
	
	public inline function getData() : Uint8ArrayData {
		return this;
	}
		
	public static function fromData( d : Uint8ArrayData ) : Uint8Array {
		return cast d;
	}
	
	public static function fromArray( a : Array<Int>, pos = 0, ?length ) : Uint8Array {
		if( length == null ) length = a.length - pos;
		if( pos < 0 || length < 0 || pos + length > a.length ) throw Error.OutsideBounds;
		#if js
		if( pos == 0 && length == a.length )
			return fromData(new Uint8ArrayData(a));
		#end
		var i = new Uint8Array(a.length);
		for( idx in 0...length )
			i[idx] = a[idx + pos];
		return i;
	}
	
}


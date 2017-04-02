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

typedef Float64ArrayData = ArrayBufferView.ArrayBufferViewData;

abstract Float64Array(Float64ArrayData) {

	public static inline var BYTES_PER_ELEMENT = 8;
	public var length(get,never) : Int;
	public var view(get,never) : ArrayBufferView;

	public inline function new( elements : Int ) {
		this = new ArrayBufferView(elements * BYTES_PER_ELEMENT).getData();
	}

	inline function get_length() {
		return this.byteLength >> 3;
	}

	public inline function get_view() : ArrayBufferView {
		return ArrayBufferView.fromData(this);
	}

	@:arrayAccess public inline function get( index : Int ) : Float {
		return this.bytes.getFloat((index<<3) + this.byteOffset);
	}

	@:arrayAccess public inline function set( index : Int, value : Float ) : Float {
		if( index >= 0 && index < length ) {
			this.bytes.setFloat((index<<3) + this.byteOffset, value);
			return value;
		}
		return 0;
	}

	public inline function sub( begin : Int, ?length : Int ) : Float64Array {
		return fromData(this.sub(begin<<3,length == null ? null : length<<3));
	}

	public inline function subarray( ?begin : Int, ?end : Int ) : Float64Array {
		return fromData(this.subarray(begin==null?null:begin<<3,end==null?null:end<<3));
	}

	public inline function getData() : Float64ArrayData {
		return this;
	}

	public static function fromData( d : Float64ArrayData ) : Float64Array {
		return cast d;
	}

	public static function fromArray( a : Array<Float>, pos = 0, ?length : Int ) : Float64Array {
		if( length == null ) length = a.length - pos;
		if( pos < 0 || length < 0 || pos + length > a.length ) throw Error.OutsideBounds;
		var i = new Float64Array(a.length);
		for( idx in 0...length )
			i[idx] = a[idx + pos];
		return i;
	}

	public static function fromBytes( bytes : haxe.io.Bytes, bytePos = 0, ?length : Int ) : Float64Array {
		return fromData(ArrayBufferView.fromBytes(bytes,bytePos,(length == null ? (bytes.length - bytePos)>>3 : length)<<3).getData());
	}
}


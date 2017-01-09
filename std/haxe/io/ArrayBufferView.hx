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

typedef ArrayBufferViewData = ArrayBufferViewImpl;

class ArrayBufferViewImpl {
	public var bytes : haxe.io.Bytes;
	public var byteOffset : Int;
	public var byteLength : Int;
	public function new(bytes, pos, length) {
		this.bytes = bytes;
		this.byteOffset = pos;
		this.byteLength = length;
	}
	public function sub( begin : Int, ?length : Int ) {
		if( length == null ) length = byteLength - begin;
		if( begin < 0 || length < 0 || begin + length > byteLength ) throw Error.OutsideBounds;
		return new ArrayBufferViewImpl(bytes, byteOffset + begin, length);
	}
	public function subarray( ?begin : Int, ?end : Int ) {
		if( begin == null ) begin = 0;
		if( end == null ) end = byteLength - begin;
		return sub(begin, end - begin);
	}
}

abstract ArrayBufferView(ArrayBufferViewData) {

	/**
		On some platforms configurations (for instance JS with no TypedArray support as in IE8-), Haxe will
		try to emulate the array buffers API. However in that case memory sharing will not be supported :
		each typed array or will copy its own data set. This flag allows users to detect if we are doing such emulation.
		At the moment only JavaScript is concerned.
	**/
	public static var EMULATED(get,never) : Bool;
	static inline function get_EMULATED() : Bool {
		return false;
	}

	public var buffer(get,never) : haxe.io.Bytes;
	public var byteOffset(get, never) : Int;
	public var byteLength(get, never) : Int;

	public inline function new( size : Int ) {
		this = new ArrayBufferViewData(haxe.io.Bytes.alloc(size), 0, size);
	}

	inline function get_byteOffset() : Int return this.byteOffset;
	inline function get_byteLength() : Int return this.byteLength;
	inline function get_buffer() : haxe.io.Bytes return this.bytes;

	public inline function sub( begin : Int, ?length : Int ) : ArrayBufferView {
		return fromData(this.sub(begin,length));
	}

	public inline function subarray( ?begin : Int, ?end : Int ) : ArrayBufferView {
		return fromData(this.subarray(begin,end));
	}

	public inline function getData() : ArrayBufferViewData {
		return this;
	}

	public static inline function fromData( a : ArrayBufferViewData ) : ArrayBufferView {
		return cast a;
	}

	public static function fromBytes( bytes : haxe.io.Bytes, pos = 0, ?length : Int ) : ArrayBufferView {
		if( length == null ) length = bytes.length - pos;
		if( pos < 0 || length < 0 || pos + length > bytes.length ) throw Error.OutsideBounds;
		return fromData(new ArrayBufferViewData(bytes, pos, length));
	}

}
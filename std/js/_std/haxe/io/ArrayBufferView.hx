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

typedef ArrayBufferViewData = js.html.ArrayBufferView;

abstract ArrayBufferView(ArrayBufferViewData) {

	public static var EMULATED(get,never) : Bool;
	static inline function get_EMULATED() {
		#if nodejs
		return false;
		#else
		return (cast js.html.ArrayBuffer) == js.html.compat.ArrayBuffer;
		#end
	}

	public var buffer(get,never) : haxe.io.Bytes;
	public var byteOffset(get, never) : Int;
	public var byteLength(get, never) : Int;

	public inline function new( size : Int ) {
		this = new js.html.Uint8Array(size);
	}

	inline function get_byteOffset() return this.byteOffset;
	inline function get_byteLength() return this.byteLength;
	inline function get_buffer() : haxe.io.Bytes {
		return haxe.io.Bytes.ofData(this.buffer);
	}

	public inline function sub( begin : Int, ?length : Int ) {
		return fromData(new js.html.Uint8Array(this.buffer.slice(begin, length == null ? null : begin+length)));
	}

	public inline function getData() : ArrayBufferViewData {
		return this;
	}

	public static inline function fromData( a : ArrayBufferViewData ) : ArrayBufferView {
		return cast a;
	}

}
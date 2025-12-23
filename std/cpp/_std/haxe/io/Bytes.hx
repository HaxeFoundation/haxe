/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import cpp.NativeArray;
import cpp.marshal.View;
import cpp.encoding.Utf8;
import cpp.encoding.Ascii;
import haxe.iterators.StringIterator;

using cpp.marshal.Marshal;
using cpp.marshal.ViewExtensions;

class Bytes {
	public var length(default, null):Int;

	final b:BytesData;

	function new(length, b) {
		this.length = length;
		this.b      = b;
	}

	/**
		Returns the byte at index `pos`.
	**/
	public function get(pos:Int):Int {
		return this.asView().slice(pos).readUInt8();
	}

	/**
		Stores the given byte `v` at the given position `pos`.
	**/
	public function set(pos:Int, v:Int):Void {
		this.asView().slice(pos).writeUInt8(v);
	}

	/**
		Copies `len` bytes from `src` into this instance.
		@param pos Zero-based location in `this` instance at which to start writing
			bytes.
		@param src Source `Bytes` instance from which to copy bytes.
		@param srcpos Zero-based location at `src` from which bytes will be copied.
		@param len Number of bytes to be copied.
	**/
	public function blit(pos:Int, src:Bytes, srcpos:Int, len:Int):Void {
		final srcView = src.asView().slice(srcpos, len);
		final dstView = this.asView().slice(pos);

		srcView.copyTo(dstView);
	}

	/**
		Sets `len` consecutive bytes starting from index `pos` of `this` instance
		to `value`.
	**/
	public function fill(pos:Int, len:Int, value:Int) {
		this.asView().slice(pos, len).fill(value);
	}

	/**
		Returns a new `Bytes` instance that contains a copy of `len` bytes of
		`this` instance, starting at index `pos`.
	**/
	public function sub(pos:Int, len:Int):Bytes {
		return this.asView().slice(pos, len).toBytes();
	}

	/**
		Returns `0` if the bytes of `this` instance and the bytes of `other` are
		identical.

		Returns a negative value if the `length` of `this` instance is less than
		the `length` of `other`, or a positive value if the `length` of `this`
		instance is greater than the `length` of `other`.

		In case of equal `length`s, returns a negative value if the first different
		value in `other` is greater than the corresponding value in `this`
		instance; otherwise returns a positive value.
	**/
	public function compare(other:Bytes):Int {
		final fst = this.asView();
		final snd = other.asView();

		return fst.compare(snd);
	}

	/**
		Returns the IEEE double-precision value at the given position `pos` (in
		little-endian encoding). Result is unspecified if `pos` is outside the
		bounds.
	**/
	public function getDouble(pos:Int):Float {
		return this.asView().slice(pos).readFloat64();
	}

	/**
		Returns the IEEE single-precision value at the given position `pos` (in
		little-endian encoding). Result is unspecified if `pos` is outside the
		bounds.
	**/
	public function getFloat(pos:Int):Float {
		return this.asView().slice(pos).readFloat32();
	}

	/**
		Stores the given IEEE double-precision value `v` at the given position
		`pos` in little-endian encoding. Result is unspecified if writing outside
		of bounds.
	**/
	public function setDouble(pos:Int, v:Float):Void {
		this.asView().slice(pos).writeFloat64(v);
	}

	/**
		Stores the given IEEE single-precision value `v` at the given position
		`pos` in little-endian encoding. Result is unspecified if writing outside
		of bounds.
	**/
	public function setFloat(pos:Int, v:Float):Void {
		this.asView().slice(pos).writeFloat32(v);
	}

	/**
		Returns the 16-bit unsigned integer at the given position `pos` (in
		little-endian encoding).
	**/
	public function getUInt16(pos:Int):Int {
		return this.asView().slice(pos).readUInt16();
	}

	/**
		Stores the given 16-bit unsigned integer `v` at the given position `pos`
		(in little-endian encoding).
	**/
	public function setUInt16(pos:Int, v:Int):Void {
		this.asView().slice(pos).writeUInt16(v);
	}

	/**
		Returns the 32-bit integer at the given position `pos` (in little-endian
		encoding).
	**/
	public function getInt32(pos:Int):Int {
		return this.asView().slice(pos).readInt32();
	}

	/**
		Returns the 64-bit integer at the given position `pos` (in little-endian
		encoding).
	**/
	public function getInt64(pos:Int):haxe.Int64 {
		return this.asView().slice(pos).readInt64();
	}

	/**
		Stores the given 32-bit integer `v` at the given position `pos` (in
		little-endian encoding).
	**/
	public function setInt32(pos:Int, v:Int):Void {
		this.asView().slice(pos).writeInt32(v);
	}

	/**
		Stores the given 64-bit integer `v` at the given position `pos` (in
		little-endian encoding).
	**/
	public function setInt64(pos:Int, v:haxe.Int64):Void {
		this.asView().slice(pos).writeInt64(v);
	}

	/**
		Returns the `len`-bytes long string stored at the given position `pos`,
		interpreted with the given `encoding` (UTF-8 by default).
	**/
	public function getString(pos:Int, len:Int, ?encoding:Encoding):String {
		return Utf8.decode(this.asView().slice(pos, len));
	}

	@:deprecated("readString is deprecated, use getString instead")
	@:noCompletion
	public function readString(pos:Int, len:Int):String {
		return getString(pos, len);
	}

	/**
		Returns a `String` representation of the bytes interpreted as UTF-8.
	**/
	public function toString():String {
		return getString(0, length);
	}

	/**
		Returns a hexadecimal `String` representation of the bytes of `this`
		instance.
	**/
	public function toHex():String {
		static final chars = [ for (c in new StringIterator("0123456789abcdef")) c ];

		final s = new StringBuf();

		for (i in 0...length) {
			final c = get(i);
			s.addChar(chars[c >> 4]);
			s.addChar(chars[c & 15]);
		}

		return s.toString();
	}

	/**
		Returns the bytes of `this` instance as `BytesData`.
	**/
	public function getData():BytesData {
		return b;
	}

	/**
		Returns a new `Bytes` instance with the given `length`. The values of the
		bytes are not initialized and may not be zero.
	**/
	public static function alloc(length:Int):Bytes {
		return new Bytes(length, cpp.NativeArray.create(length));
	}

	/**
		Returns the `Bytes` representation of the given `String`, using the
		specified encoding (UTF-8 by default).
	**/
	@:pure
	public static function ofString(s:String, ?encoding:Encoding):Bytes {
		if (Ascii.isEncoded(s)) {
			return s.asCharView().asBytesView().toBytes();
		} else {
			final count = Utf8.getByteCount(s);
			final bytes = Bytes.alloc(Int64.toInt(count));

			if (Utf8.encode(s, bytes.asView()) != count) {
				throw new haxe.Exception('Failed to encode string to UTF8');
			} else {
				return bytes;
			}
		}
	}

	/**
		Returns the `Bytes` representation of the given `BytesData`.
	**/
	public static function ofData(b:BytesData) {
		return new Bytes(b.length, b);
	}

	/**
		Converts the given hexadecimal `String` to `Bytes`. `s` must be a string of
		even length consisting only of hexadecimal digits. For example:
		`"0FDA14058916052309"`.
	**/
	public static function ofHex(s:String):Bytes {
		if ((s.length & 1) != 0) {
			throw new haxe.exceptions.ArgumentException("s", "Not a hex string (odd number of digits)");
		}

		final output = Bytes.alloc(s.length >> 1);
		for (i in 0...output.length) {
			final highCode = StringTools.fastCodeAt(s, i * 2);
			final lowCode  = StringTools.fastCodeAt(s, i * 2 + 1);
			final high     = (highCode & 0xF) + ((highCode & 0x40) >> 6) * 9;
			final low      = (lowCode & 0xF) + ((lowCode & 0x40) >> 6) * 9;

			output.set(i, ((high << 4) | low) & 0xFF);
		}

		return output;
	}

	/**
		Reads the `pos`-th byte of the given `b` bytes, in the most efficient way
		possible. Behavior when reading outside of the available data is
		unspecified.
	**/
	public static function fastGet(b:BytesData, pos:Int):Int {
		return b.asView().ptr[pos];
	}
}

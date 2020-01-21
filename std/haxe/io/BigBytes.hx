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

import haxe.errors.NotImplemented;

/**
	TODO:
	This is an attempt to design a cross-platform API for big byte buffers (more than 2GB)
	without any unnecessary allocations.
**/
class BigBuffer {
	public function getLength():Int64 {
		throw new NotImplemented();
	}

	/**
		Move internal pointer to the beginning - to the byte at index 0.
	**/
	public function rewind():Void {
		throw new NotImplemented();
	}

	/**
		Move internal pointer past the last byte.
	**/
	public function fastForward():Void {
		throw new NotImplemented();
	}

	/**
		Move internal pointer by `step` bytes forward (if `step` is positive)
		or backward (if `step` is negative)
	**/
	static public inline function shift(step:Int):Void {
		throw new NotImplemented();
	}

	/**
		Copy up to `length` bytes from this buffer starting at the internal
		pointer position into `buffer` starting at `offset`.

		Returns amount of bytes copied.

		Advances internal pointer by the return value.
	**/
	static public function copyTo(buffer:Bytes, offset:Int, length:Int):Int {
		throw new NotImplemented();
	}

	/**
		Copy up to `length` bytes from `buffer` starting at `offset` into this
		buffer starting at the internal pointer position.

		Returns amount of bytes copied.

		Advances internal pointer by the return value.
	**/
	static public function copyFrom(buffer:Bytes, offset:Int, length:Int):Int {
		throw new NotImplemented();
	}

	/**
		Sets up to `length` consecutive bytes starting from internal pointer position
		to `value`.

		Returns amount of bytes filled.

		Advances internal pointer by the return value.
	**/
	public function fill(length:Int, value:Int):Int {
		throw new NotImplemented();
	}

	/**
		Returns a new `Bytes` instance that contains a copy of up to `length` bytes of
		`this` instance, starting at the internal pointer position.

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by the amount of bytes returned.
	**/
	public function sub(length:Int):Bytes {
		throw new NotImplemented();
	}

	/**
		Returns the IEEE double-precision value at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function getDouble():Float {
		throw new NotImplemented();
	}

	/**
		Returns the IEEE single-precision value at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function getFloat():Float {
		throw new NotImplemented();
	}

	/**
		Stores the given IEEE double-precision value `v` at the internal pointer
		position in little-endian encoding.

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function setDouble(v:Float):Void {
		throw new NotImplemented();
	}

	/**
		Stores the given IEEE single-precision value `v` at the internal pointer
		position in little-endian encoding.

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function setFloat(v:Float):Void {
		throw new NotImplemented();
	}

	/**
		Returns the 16-bit unsigned integer at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 2 bytes.
	**/
	public inline function getUInt16(pos:Int):Int {
		throw new NotImplemented();
	}

	/**
		Stores the given 16-bit unsigned integer `v` at the given position `pos`
		(in little-endian encoding).
	**/
	public inline function setUInt16(pos:Int, v:Int):Void {
		#if neko_v21
		untyped $sset16(b, pos, v, false);
		#else
		set(pos, v);
		set(pos + 1, v >> 8);
		#end
	}

	/**
		Returns the 32-bit integer at the given position `pos` (in little-endian
		encoding).
	**/
	public inline function getInt32(pos:Int):Int {
		#if neko_v21
		return untyped $sget32(b, pos, false);
		#elseif python
		var v = get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos + 3) << 24);
		return if (v & 0x80000000 != 0) v | 0x80000000 else v;
		#elseif lua
		var v = get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos + 3) << 24);
		return lua.Boot.clampInt32(if (v & 0x80000000 != 0) v | 0x80000000 else v);
		#else
		return get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos + 3) << 24);
		#end
	}

	/**
		Returns the 64-bit integer at the given position `pos` (in little-endian
		encoding).
	**/
	public inline function getInt64(pos:Int):haxe.Int64 {
		return haxe.Int64.make(getInt32(pos + 4), getInt32(pos));
	}

	/**
		Stores the given 32-bit integer `v` at the given position `pos` (in
		little-endian encoding).
	**/
	public inline function setInt32(pos:Int, v:Int):Void {
		#if neko_v21
		untyped $sset32(b, pos, v, false);
		#else
		set(pos, v);
		set(pos + 1, v >> 8);
		set(pos + 2, v >> 16);
		set(pos + 3, v >>> 24);
		#end
	}

	/**
		Stores the given 64-bit integer `v` at the given position `pos` (in
		little-endian encoding).
	**/
	public inline function setInt64(pos:Int, v:haxe.Int64):Void {
		setInt32(pos, v.low);
		setInt32(pos + 4, v.high);
	}

	/**
		Returns the `len`-bytes long string stored at the given position `pos`,
		interpreted with the given `encoding` (UTF-8 by default).
	**/
	public function getString(pos:Int, len:Int, ?encoding:Encoding):String {
		if (encoding == null)
			encoding == UTF8;
		#if !neko
		if (pos < 0 || len < 0 || pos + len > length)
			throw Error.OutsideBounds;
		#end
		#if neko
		return try new String(untyped __dollar__ssub(b, pos, len)) catch (e:Dynamic) throw Error.OutsideBounds;
		#elseif flash
		b.position = pos;
		return encoding == RawNative ? b.readMultiByte(len, "unicode") : b.readUTFBytes(len);
		#elseif cpp
		var result:String = "";
		untyped __global__.__hxcpp_string_of_bytes(b, result, pos, len);
		return result;
		#elseif cs
		switch (encoding) {
			case UTF8 | null:
				return cs.system.text.Encoding.UTF8.GetString(b, pos, len);
			case RawNative:
				return cs.system.text.Encoding.Unicode.GetString(b, pos, len);
		}
		#elseif java
		try {
			switch (encoding) {
				case UTF8 | null:
					return new String(b, pos, len, "UTF-8");
				case RawNative:
					return new String(b, pos, len, "UTF-16LE");
			}
		} catch (e:Dynamic) {
			throw e;
		}
		#elseif python
		return python.Syntax.code("self.b[{0}:{0}+{1}].decode('UTF-8','replace')", pos, len);
		#elseif lua
		if (b.length - pos <= lua.Boot.MAXSTACKSIZE) {
			var end:Int = cast Math.min(b.length, pos + len) - 1;
			return lua.NativeStringTools.char(lua.TableTools.unpack(untyped b, pos, end));
		} else {
			var tbl:lua.Table<Int, String> = lua.Table.create();
			for (idx in pos...pos + len) {
				lua.Table.insert(tbl, lua.NativeStringTools.char(b[idx]));
			}
			return lua.Table.concat(tbl, '');
		}
		#else
		var s = "";
		var b = b;
		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos + len;
		// utf8-decode and utf16-encode
		while (i < max) {
			var c = b[i++];
			if (c < 0x80) {
				if (c == 0)
					break;
				s += fcc(c);
			} else if (c < 0xE0)
				s += fcc(((c & 0x3F) << 6) | (b[i++] & 0x7F));
			else if (c < 0xF0) {
				var c2 = b[i++];
				s += fcc(((c & 0x1F) << 12) | ((c2 & 0x7F) << 6) | (b[i++] & 0x7F));
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				var u = ((c & 0x0F) << 18) | ((c2 & 0x7F) << 12) | ((c3 & 0x7F) << 6) | (b[i++] & 0x7F);
				// surrogate pair
				s += fcc((u >> 10) + 0xD7C0);
				s += fcc((u & 0x3FF) | 0xDC00);
			}
		}
		return s;
		#end
	}

	@:deprecated("readString is deprecated, use getString instead")
	@:noCompletion
	public inline function readString(pos:Int, len:Int):String {
		return getString(pos, len);
	}

	/**
		Returns a `String` representation of the bytes interpreted as UTF-8.
	**/
	public function toString():String {
		#if neko
		return new String(untyped __dollar__ssub(b, 0, length));
		#elseif flash
		b.position = 0;
		return b.toString();
		#elseif cs
		return cs.system.text.Encoding.UTF8.GetString(b, 0, length);
		#elseif java
		try {
			return new String(b, 0, length, "UTF-8");
		} catch (e:Dynamic)
			throw e;
		#else
		return getString(0, length);
		#end
	}

	/**
		Returns a hexadecimal `String` representation of the bytes of `this`
		instance.
	**/
	public function toHex():String {
		var s = new StringBuf();
		var chars = [];
		var str = "0123456789abcdef";
		for (i in 0...str.length)
			chars.push(str.charCodeAt(i));
		for (i in 0...length) {
			var c = get(i);
			s.addChar(chars[c >> 4]);
			s.addChar(chars[c & 15]);
		}
		return s.toString();
	}

	/**
		Returns the bytes of `this` instance as `BytesData`.
	**/
	public inline function getData():BytesData {
		return b;
	}

	/**
		Returns a new `Bytes` instance with the given `length`. The values of the
		bytes are not initialized and may not be zero.
	**/
	public static function alloc(length:Int):Bytes {
		#if neko
		return new Bytes(length, untyped __dollar__smake(length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.length = length;
		return new Bytes(length, b);
		#elseif cpp
		var a = new BytesData();
		if (length > 0)
			cpp.NativeArray.setSize(a, length);
		return new Bytes(length, a);
		#elseif cs
		return new Bytes(length, new cs.NativeArray(length));
		#elseif java
		return new Bytes(length, new java.NativeArray(length));
		#elseif python
		return new Bytes(length, new python.Bytearray(length));
		#else
		var a = new Array();
		for (i in 0...length)
			a.push(0);
		return new Bytes(length, a);
		#end
	}

	/**
		Returns the `Bytes` representation of the given `String`, using the
		specified encoding (UTF-8 by default).
	**/
	@:pure
	public static function ofString(s:String, ?encoding:Encoding):Bytes {
		#if neko
		return new Bytes(s.length, untyped __dollar__ssub(s.__s, 0, s.length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		if (encoding == RawNative)
			b.writeMultiByte(s, "unicode")
		else
			b.writeUTFBytes(s);
		return new Bytes(b.length, b);
		#elseif cpp
		var a = new BytesData();
		untyped __global__.__hxcpp_bytes_of_string(a, s);
		return new Bytes(a.length, a);
		#elseif cs
		var b = switch (encoding) {
			case UTF8 | null:
				cs.system.text.Encoding.UTF8.GetBytes(s);
			case RawNative:
				cs.system.text.Encoding.Unicode.GetBytes(s);
		};
		return new Bytes(b.Length, b);
		#elseif java
		try {
			var b:BytesData = switch (encoding) {
				case UTF8 | null:
					@:privateAccess s.getBytes("UTF-8");
				case RawNative:
					@:privateAccess s.getBytes("UTF-16LE");
			};
			return new Bytes(b.length, b);
		} catch (e:Dynamic) {
			throw e;
		}
		#elseif python
		var b:BytesData = new python.Bytearray(s, "UTF-8");
		return new Bytes(b.length, b);
		#elseif lua
		var bytes = [
			for (i in 0...lua.NativeStringTools.len(s)) {
				lua.NativeStringTools.byte(s, i + 1);
			}
		];
		return new Bytes(bytes.length, bytes);
		#else
		var a = new Array();
		// utf16-decode and utf8-encode
		var i = 0;
		while (i < s.length) {
			var c:Int = StringTools.fastCodeAt(s, i++);
			// surrogate pair
			if (0xD800 <= c && c <= 0xDBFF)
				c = (c - 0xD7C0 << 10) | (StringTools.fastCodeAt(s, i++) & 0x3FF);
			if (c <= 0x7F)
				a.push(c);
			else if (c <= 0x7FF) {
				a.push(0xC0 | (c >> 6));
				a.push(0x80 | (c & 63));
			} else if (c <= 0xFFFF) {
				a.push(0xE0 | (c >> 12));
				a.push(0x80 | ((c >> 6) & 63));
				a.push(0x80 | (c & 63));
			} else {
				a.push(0xF0 | (c >> 18));
				a.push(0x80 | ((c >> 12) & 63));
				a.push(0x80 | ((c >> 6) & 63));
				a.push(0x80 | (c & 63));
			}
		}
		return new Bytes(a.length, a);
		#end
	}

	/**
		Returns the `Bytes` representation of the given `BytesData`.
	**/
	public static function ofData(b:BytesData) {
		#if flash
		return new Bytes(b.length, b);
		#elseif neko
		return new Bytes(untyped __dollar__ssize(b), b);
		#elseif cs
		return new Bytes(b.Length, b);
		#else
		return new Bytes(b.length, b);
		#end
	}

	/**
		Converts the given hexadecimal `String` to `Bytes`. `s` must be a string of
		even length consisting only of hexadecimal digits. For example:
		`"0FDA14058916052309"`.
	**/
	public static function ofHex(s:String):Bytes {
		var len:Int = s.length;
		if ((len & 1) != 0)
			throw "Not a hex string (odd number of digits)";
		var ret:Bytes = Bytes.alloc(len >> 1);
		for (i in 0...ret.length) {
			var high = StringTools.fastCodeAt(s, i * 2);
			var low = StringTools.fastCodeAt(s, i * 2 + 1);
			high = (high & 0xF) + ((high & 0x40) >> 6) * 9;
			low = (low & 0xF) + ((low & 0x40) >> 6) * 9;
			ret.set(i, ((high << 4) | low) & 0xFF);
		}

		return ret;
	}

	/**
		Reads the `pos`-th byte of the given `b` bytes, in the most efficient way
		possible. Behavior when reading outside of the available data is
		unspecified.
	**/
	public inline static function fastGet(b:BytesData, pos:Int):Int {
		#if neko
		return untyped __dollar__sget(b, pos);
		#elseif flash
		return b[pos];
		#elseif cpp
		return untyped b.unsafeGet(pos);
		#elseif java
		return untyped b[pos] & 0xFF;
		#else
		return b[pos];
		#end
	}
}

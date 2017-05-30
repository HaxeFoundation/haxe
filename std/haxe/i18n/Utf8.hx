/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.i18n;

import haxe.i18n.Tools;

import haxe.io.Bytes;

@:structInit private class Utf8Impl {
	public var b(default,null) : ByteAccess;
	public var length(default,null) : Int;
}

@:allow(haxe.i18n.Utf8Tools)
abstract Utf8(Utf8Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = Utf8Tools.nativeStringToImpl(str);
	}

	public inline function toUpperCase() : Utf8 {
		return fromImpl(Utf8Tools.toUpperCase(this));
	}

	public inline function toLowerCase() : Utf8 {
		return fromImpl(Utf8Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Utf8 {
		return fromImpl(Utf8Tools.charAt(this, index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf8Tools.charCodeAt(this, index);
	}

	public inline function fastCodeAt( index : Int) : Null<Int> {
		return Utf8Tools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf8, ?startIndex : Int ) : Int {
		 return Utf8Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		return Utf8Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Utf8 ) : Array<Utf8> {
		return Utf8Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Utf8 {
		return fromImpl(Utf8Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		return fromImpl(Utf8Tools.substring(this, startIndex, endIndex));
	}

	public static inline function fromCharCode( code : Int ) : Utf8 {
		return fromImpl(Utf8Tools.fromCharCode(code));
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromImpl(Utf8Tools.append(this, other.impl()));
	}

	@:op(A == B) inline function opEq (other:Utf8) {
		return Utf8Tools.equal(this, other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}

	@:op(A > B) inline function opGreaterThan (other:Utf8) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf8) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf8) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf8) {
		return compare(other) >= 0;
	}

	// additional public api

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public inline function toNativeString() : String {
		return Utf8Tools.toNativeString(this);
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromByteAccess(Convert.convertUtf8toUcs2(getReader(), true, false));
	}

 	public inline function toUtf16 ():Utf16 {
		return Utf16.fromByteAccess(Convert.convertUtf8toUtf16(getReader(), true));
	}

	public inline function toUtf32 ():Utf32 {
		return Utf32.fromByteAccess(Convert.convertUtf8toUtf32(getReader(), true));
	}

	public inline function toBytes() : haxe.io.Bytes {
		return Utf8Tools.toBytes(this);
	}

	public inline function toCodeArray ():Array<Int> {
		return Utf8Tools.toCodeArray(this);
	}

	public inline function eachCode (f:Int->Void):Void {
		Utf8Tools.eachCode(this, f);
	}

	public static inline function fromByteAccess (ba:ByteAccess) {
		return fromImpl(Utf8Tools.fromByteAccess(ba));
	}

	public inline function getReader ():Utf8Reader {
		return new Utf8Reader(this.b);
	}

	// private api

	inline function get_length() {
		return Utf8Tools.strLength(this);
	}

	inline function compare (other:Utf8):Int {
		return Utf8Tools.compare(this, other.impl());
	}

	// wrap and unwrap

	inline function impl ():Utf8Impl {
		return this;
	}

	static inline function fromImpl (impl:Utf8Impl):Utf8 {
		return cast impl;
	}
}

abstract Utf8Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	public inline function fastGet (pos:Int) {
		return this.fastGet(pos);
	}
}

@:publicFields
private class Utf8Tools {

	// implementation specific

	static inline function fastGet (ba:Utf8Impl, pos:Int) {
		return ba.b.fastGet(pos);
	}

	static inline function set (ba:Utf8Impl, pos:Int, val:Int) {
		return ba.b.set(pos, val);
	}

	static inline function byteLength (ba:Utf8Impl) {
		return ba.b.length;
	}

	static inline function strLength (ba:Utf8Impl) {
		return ba.length;
	}

	static inline function mkImpl (b:ByteAccess, len:Int):Utf8Impl {
		return { b : b, length: len };
	}

	static inline function mkImplFromBuffer(buf:ByteAccessBuffer, newSize:Int):Utf8Impl {
		return mkImpl(buf.getByteAccess(), newSize);
	}

	static inline function sub (ba:Utf8Impl, pos:Int, size:Int, newLen:Int):Utf8Impl {
		if (newLen == 0 && size == 0) return empty;
		var bytes = ba.b.sub(pos, size);
		return mkImpl(bytes, newLen);
	}

	static inline function allocImpl (size:Int, strLength:Int):Utf8Impl {
		return mkImpl(ByteAccess.alloc(size), strLength);
	}

	static inline function nativeStringToImpl (s:String):Utf8Impl {
		if (s.length == 0) return empty;
		var ba = nativeStringToByteAccess(s);
		return mkImpl(ba, calcLength(ba));
	}

	static inline function append (ba:Utf8Impl, other:Utf8Impl):Utf8Impl {
		if (other.length == 0) return ba;
		if (ba.length == 0) return other;

		return mkImpl(ba.b.append(other.b), ba.length + other.length);
	}

	static inline function equal (ba:Utf8Impl, other:Utf8Impl):Bool {
		return ba == other || (ba.length == other.length && ba.b.equal(other.b));
	}

	static inline function toBytes(impl:Utf8Impl) : haxe.io.Bytes {
		return impl.b.copy().toBytes();
	}

	static inline function fromByteAccess (ba:ByteAccess):Utf8Impl {
		if (!Convert.isLegalUtf8Source(new Utf8Reader(ba))) {
			throw "illegal utf8";
		}
		var len = calcLength(ba);
		return mkImpl(ba, len);
	}

	public static inline function toNativeString(impl:Utf8Impl) : String {
		return impl.b.getString(0, impl.b.length);
	}

	// end implementation specific

	// helper functions

	static function calcLength(ba:ByteAccess) {
		var len = 0;
		var i = 0;
		while (i < ba.length) {
			var size = getSequenceSize(ba.fastGet(i));
			len++;
			i += size;
		}
		return len;
	}

	static inline function getSequenceSize (start:Int):Int {
		return Convert.getUtf8SequenceSize(start);
	}

	static inline function isUpperCaseLetter (bytes:Utf8Impl, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x41 && b <= 0x5A;
	}

	static inline function isLowerCaseLetter (bytes:Utf8Impl, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x61 && b <= 0x7A;
	}

	static inline function toLowerCaseLetter (bytes:Utf8Impl, target:Utf8Impl, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)+0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	static inline function toUpperCaseLetter (bytes:Utf8Impl, target:Utf8Impl, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)-0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	static var empty = allocImpl(0, 0);

	static inline function eachCode ( ba:Utf8Impl, f : Int -> Void) {
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getSequenceSize(b);
			var code = getCharCode(ba, i, size);
			f(code);
			i += size;
		}
	}

	static inline function getCharCode ( b:Utf8Impl, pos:Int, size:Int):Int {
		return Convert.charCodeFromUtf8Bytes(new Utf8Reader(b.b), pos, size);
	}

	static inline function compareChar ( b1:Utf8Impl, pos1:Int, b2:Utf8Impl, pos2:Int, size:Int):Bool {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 == c2;
	}

	static inline function pushCharCode (bytes:Utf8Impl, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(fastGet(bytes, pos+i));
		}
	}

	static inline function nativeStringToByteAccess (s:String):ByteAccess {
		return NativeStringTools.toUtf8ByteAccess(s);
 	}

	// string functions

	static function map(ba:Utf8Impl, f:Utf8Impl->Utf8Impl->Int->Int->Void) : Utf8Impl {
		var res = allocImpl(byteLength(ba), strLength(ba));
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getSequenceSize(b);
			f(ba, res, i, size);

			i += size;
		}
		return res;
	}

	static inline function toUpperCase(ba:Utf8Impl) : Utf8Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return map(ba, function (impl, res, i, size) return toUpperCaseLetter(impl, res, i, size));
	}

	static inline function toLowerCase(ba:Utf8Impl) : Utf8Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return map(ba, function (impl, res, i, size) return toLowerCaseLetter(impl, res, i, size));
	}

	static function charAt(ba:Utf8Impl, index : Int) : Utf8Impl {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getSequenceSize(b);
			if (pos == index) {
				res = sub(ba, i, size, 1);
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty : res;
	}

	static function charCodeAt( ba:Utf8Impl, index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < byteLength(ba)) {

			var b = fastGet(ba, i);
			var size = getSequenceSize(b);
			if (pos == index) {
				r = getCharCode(ba, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return r;
	}

	static function fastCodeAt( ba:Utf8Impl, index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;

		while (i < byteLength(ba)) {

			var b = fastGet(ba, i);
			var size = getSequenceSize(b);
			if (pos == index) {
				return getCharCode(ba, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return 0;
	}

	static function indexOf( ba:Utf8Impl, str : Utf8Impl, ?startIndex : Int ) : Int
	{
		var strLen = strLength(str);
		var res = -1;
		var pos = 0;
		var posFull = 0;
		var byteLength = byteLength(ba);
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		// move forward to startIndex
		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getSequenceSize(fastGet(ba, i));
				i += size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size1 = getSequenceSize(fastGet(ba, i));
			var size2 = getSequenceSize(fastGet(str, j));

			if (size1 == size2 && compareChar(ba, i, str, j, size1)) {
				pos++;
				j += size1;
			} else {
				j = 0;
				posFull += pos;
				pos = 0;
			}

			i += size1;
			if (pos == strLen) {
				res = posFull;
				break;
			}
			if (pos == 0) {
				posFull++;
			}
		}
		return res;
	}

	static function lastIndexOf( ba:Utf8Impl, str : Utf8Impl, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var res = -1;
		var len = strLength(str);
		var pos = 0;
		var posFull = 0;

		var i = 0;
		var j = 0;

		var iNext = 0;

		while (i < byteLength(ba) && (startIndexIsNull || posFull < startIndex + 1)) {
			var size1 = getSequenceSize(fastGet(ba, i));
			var size2 = getSequenceSize(fastGet(str, j));
			if (size1 == size2 && compareChar(ba, i, str, j, size1)) {
				if (j == 0) {
					// store the next position character position in bytes for next search
					iNext = i + size1;
				}
				pos++;
				j += size1;
			} else {
				if (j > 0) {
					// restore next search position and continue
					posFull++;
					i = iNext;
					j = 0;
					pos = 0;
					continue;
				}
			}
			if (pos == len) {
				// store result
				res = posFull;
				// restore next search position and continue
				posFull++;
				i = iNext;
				j = 0;
				pos = 0;
				continue;
			}
			if (pos == 0) {
				posFull++;
			}
			i += size1;
		}
		return res;
	}

	static function split( str:Utf8Impl, delimiter : Utf8Impl ) : Array<Utf8>
	{
		var delimiterLen = strLength(delimiter);

		if (delimiterLen == 0) {
			var res = [];
			var i = 0;
			while ( i < byteLength(str)) {
				var size = getSequenceSize(fastGet(str, i));
				res.push(Utf8.fromImpl(sub(str, i, size, 1)));
				i+=size;
			}
			return res;
		} else {
			var res = [];
			var buf = new ByteAccessBuffer();
			var tmpBuf = new ByteAccessBuffer();
			var bufLen = 0; // store utf8 len
			var tmpBufLen = 0; // store utf8 len
			var pos = 0;
			var posFull = 0;
			var i = 0;
			var j = 0;
			// iterate bytes
			while (i < byteLength(str)) {
				var size = getSequenceSize(fastGet(str, i));
				var size2 = getSequenceSize(fastGet(delimiter, j));
				if (size == size2 && compareChar(str, i, delimiter, j, size)) {

					pos++;
					j += size;
					tmpBufLen++;
					for (k in 0...size) {
						tmpBuf.addByte(fastGet(str, i + k));
					}
				} else {
					if (pos != 0) {
						j = 0;
						pos = 0;
						buf.addBuffer(tmpBuf);
						bufLen += tmpBufLen;
						tmpBufLen = 0;
						tmpBuf = tmpBuf.reset();
					}
					for (k in 0...size) {
						buf.addByte(fastGet(str, i+k));
					}
					bufLen++;
				}
				i += size;
				if (pos == delimiterLen) {
					if (buf.length > 0) {
						res.push(Utf8.fromImpl(mkImplFromBuffer(buf, bufLen)));
						bufLen = 0;
						buf = buf.reset();
					} else {
						res.push(Utf8.fromImpl(empty));
					}
					tmpBuf = tmpBuf.reset();
					tmpBufLen = 0;
					j = 0;
					pos = 0;
				}
				posFull++;
			}
			if (pos != 0) {
				j = 0;
				pos = 0;
				buf.addBuffer(tmpBuf);
				bufLen += tmpBufLen;
			}
			if (buf.length > 0) {
				res.push(Utf8.fromImpl(mkImplFromBuffer(buf, bufLen)));
			} else {
				res.push(Utf8.fromImpl(empty));
			}
			return res;
		}
	}

	static function substr<T>( str:Utf8Impl, pos : Int, ?len : Int ) : Utf8Impl {

		if (len == 0) return empty;

		var byteLength = byteLength(str);
		if (pos < 0) {
			pos = strLength(str) + pos;
			if (pos < 0) pos = 0;
		}

		if (len != null && len < 0) {
			len = strLength(str) + len;
			if (len < 0) len = 0;
		}

		var buf = new ByteAccessBuffer();
		var cur = 0;

		var i = 0;
		var newLen = 0;
		while (i < byteLength) {
			var char = fastGet(str, i);
			var size = getSequenceSize(char);
			if (cur >= pos && (len == null || cur < pos + len))
			{
				newLen++;
				pushCharCode(str, buf, i, size);
			} else if (len != null && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return mkImplFromBuffer(buf, newLen);
	}

	static inline function substring<T>( ba:Utf8Impl, startIndex : Int, ?endIndex : Int ) : Utf8Impl {
		var startIndex:Null<Int> = startIndex;
		var len = strLength(ba);

		if (startIndex < 0) startIndex = 0;
		if (endIndex != null && endIndex < 0) endIndex = 0;

		if (endIndex == null) endIndex = len;
 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;

		return substr(ba, startIndex, endIndex - startIndex);
	}

	static inline function fromCharCode( code : Int ) : Utf8Impl
	{
		var b = Convert.charCodeToUtf8ByteAccess(code);
		return mkImpl(b, 1);
	}

	static inline function toCodeArray (ba:Utf8Impl):Array<Int> {
		var res = [];
		eachCode(ba, function (c) res.push(c));
		return res;
	}

	static inline function compare (impl:Utf8Impl, other:Utf8Impl):Int {
		return impl.b.compare(other.b);
	}
}
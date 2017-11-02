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

#if (flash || js || hl || java || cs)

import haxe.i18n.Tools;

@:structInit private class Utf16Impl {
	public var s(default, null) : String;
	public var length(default,null) : Int;
}

class Utf16Iterator {
	var s:Utf16Impl;
	var p:Int;

	public inline function new (s:Utf16) {
		this.p = 0;
		this.s = s.impl();
	}
	public inline function hasNext ():Bool {
		return p < Utf16Tools.lengthInt16(s);
	}

	public inline function next ():Int {
		var b = Utf16Tools.getInt16(s, p);
		var size = Utf16Tools.getSequenceSize(b);
		var code = Utf16Tools.getCharCode(s, p, size);
		p += size;
		return code;
	}
}

@:allow(haxe.i18n)
abstract Utf16(Utf16Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = Utf16Tools.nativeStringToImpl(str);
	}

	public inline function iterator () {
		return new Utf16Iterator(fromImpl(this));
	}

	public inline function toUpperCase() : Utf16 {
		return fromImpl(Utf16Tools.toUpperCase(this));
	}
	public inline function toLowerCase() : Utf16 {
		return fromImpl(Utf16Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Utf16 {
		return fromImpl(Utf16Tools.charAt(this, index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf16Tools.charCodeAt(this, index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return Utf16Tools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf16 ) : Array<Utf16> {
		return Utf16Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substring(this, startIndex, endIndex));
	}

	public static inline function fromCharCode( code : Int ) : Utf16 {
		return fromImpl(Utf16Tools.fromCharCode(code));
	}

	@:op(A + B) inline function opAdd (other:Utf16):Utf16 {
		return fromImpl(Utf16Tools.append(this, other.impl()));
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return Utf16Tools.equal(this, other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	@:op(A > B) inline function opGreaterThan (other:Utf16) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf16) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf16) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf16) {
		return compare(other) >= 0;
	}

	// additional public api

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public inline function toNativeString() : String {
		return this.s;
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromUtf16(fromImpl(this));
	}

	public inline function toUtf8 ():Utf8 {
		return Utf8.fromUtf16(fromImpl(this));
	}

	public inline function toUtf32 ():Utf32 {
		return Utf32.fromUtf16(fromImpl(this));
	}

	public inline function toBytes() : haxe.io.Bytes {
		return toUcs2().toBytes();
	}

	public inline function toCodeArray () {
		return Utf16Tools.toCodeArray(this);
	}

	public static inline function fromByteAccess (ba:ByteAccess) {
		return fromImpl(Utf16Tools.fromByteAccess(ba));
	}

	public static inline function fromUtf8 (s:Utf8):Utf16 {
		return fromByteAccess(Convert.convertUtf8toUtf16(s.getReader(), true));
	}

	public static inline function fromUcs2 (s:Ucs2):Utf16 {
		#if (flash || js || hl || java || cs)
		return fromImpl(Utf16Tools.nativeStringToImpl(s.toNativeString()));
		#else
		// we can reuse the same underlying byteaccess because ucs2 allows supplementary chars
		return fromByteAccess(s.impl());
		#end
	}

	public static inline function fromUtf32 (s:Utf32):Utf16 {
		#if python
		return fromByteAccess(NativeStringTools.toUtf16ByteAccess(s.impl()));
		#else
		return fromByteAccess(Convert.convertUtf32toUtf16(s.getReader(), true));
		#end
	}

	public inline function getReader ():Utf16Reader
	{
		return new Utf16Reader(this.s);
	}

	public inline function eachCode ( f : Int -> Void) {
		return Utf16Tools.eachCode(this, f);
	}


	// private helpers

	inline function get_length() {
		return Utf16Tools.strLength(this);
	}

	static inline function fromImpl (impl:Utf16Impl):Utf16 {
		return cast impl;
	}

	inline function impl ():Utf16Impl {
		return this;
	}

	inline function compare (other:Utf16):Int {
		return Utf16Tools.compare(this, other.impl());
	}
}

abstract Utf16Reader(String) {

	public inline function new (bytes:String) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	//public inline function fastGet (pos:Int) {
	//	return StringTools.fastCodeAt(this, pos);
	//}

	public inline function getInt16 (pos:Int) {
		return StringTools.fastCodeAt(this, pos);
	}
}

@:publicFields
private class Utf16Tools {

	static inline function getInt16 (impl:Utf16Impl, pos:Int) {
		return StringTools.fastCodeAt(impl.s, pos);
	}

	static inline function lengthInt16 (ba:Utf16Impl) {
		return ba.s.length;
	}

	static inline function strLength(impl:Utf16Impl) {
		return impl.length;
	}

	static inline function mkImpl (s:String, length:Int):Utf16Impl {
		return { s : s, length : length }
	}

	static inline function mkImplFromBuffer(buf:StringBuf, newSize:Int):Utf16Impl {
		return mkImpl(buf.toString(), newSize);
	}

	static inline function sub (ba:Utf16Impl, pos:Int, size:Int, newLen:Int):Utf16Impl {
		var s = ba.s.substr(pos, size);
		return mkImpl(s, newLen);
	}

	static inline function nativeStringToImpl (s:String):Utf16Impl {
		if (s.length == 0) return empty;
		return mkImpl(s, calcLength(s));
	}

	/*
	static inline function allocImpl (size:Int, strLength:Int):Utf16Impl {
		return mkImpl(ByteAccess.alloc(size), strLength);
	}
	*/

	static inline function getInt32 (impl:Utf16Impl, pos:Int) {
		var a = StringTools.fastCodeAt(impl.s, pos);
		var b = StringTools.fastCodeAt(impl.s, pos+1);
		return a | b;
	}


	static inline function fromByteAccess (ba:ByteAccess):Utf16Impl {
		var s = Ucs2.fromByteAccess(ba).toNativeString();

		var len = calcLength(s);

		return mkImpl(s, len);
	}

	static inline function equal (impl:Utf16Impl, other:Utf16Impl) {
		return impl == other || (impl.length == other.length && impl.s == other.s);
	}

	static inline function append (impl:Utf16Impl, other:Utf16Impl):Utf16Impl {
		if (other.length == 0) return impl;
		if (impl.length == 0) return other;
		return mkImpl(impl.s + other.s, impl.length + other.length);
	}

	// helper functions

	static function calcLength(str:String) {
		var len = 0;
		var index = 0;
		while (index < str.length) {
			var code = StringTools.fastCodeAt(str, index);
			var size = getSequenceSize(code);
			len++;
			index += size;
		}
		return len;
	}

	static inline function getSequenceSize (firstInt16:Int):Int {
		return if (Convert.isHighSurrogate(firstInt16)) 2 else 1;
	}

	static var empty = mkImpl("", 0);

	static inline function getCharCode ( b:Utf16Impl, pos:Int, size:Int):Int {
		return switch size {
			case 1: getInt16(b, pos);
			case 2: Convert.surrogatePairToCharCode(getInt16(b, pos), getInt16(b, pos+1));
			case _: throw "invalid size, 1 or 2 expected";
		}
	}

	static inline function compareChar ( b1:Utf16Impl, pos1:Int, b2:Utf16Impl, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	static inline function pushCharCode (impl:Utf16Impl, buf:StringBuf, pos:Int, size:Int) {
		#if hl
		var code = getCharCode(impl, pos, size);
		buf.addChar(code);
		#else
		for (i in 0...size) {
			buf.addChar(getInt16(impl, pos+i));
		}
		#end
	}

	 // string functions

	/*
	static inline function map(impl:Utf16Impl, f:Utf16Impl->Utf16Impl->Int->Int->Void) : Utf16Impl {
		var res = allocImpl(byteLength(impl), strLength(impl));
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getSequenceSize(b);
			f(impl, res, i, size);
			i += size;
		}
		return res;
	}
	*/

	static inline function toUpperCase(impl:Utf16Impl) : Utf16Impl {
		return mkImpl(impl.s.toUpperCase(), impl.length);
	}

	static inline function toLowerCase(impl:Utf16Impl) : Utf16Impl {
		return mkImpl(impl.s.toLowerCase(), impl.length);
	}

	static function charAt(impl:Utf16Impl, index : Int) : Utf16Impl {
		if (index > 0 && index < strLength(impl)) {

			if (hasNoSurrogates(impl)) {
				return sub(impl, index, 1, 1);
			}
			if (hasOnlySurrogates(impl)) {
				return sub(impl, index << 1, 2, 1);
			}
		}
		var pos = 0;
		var i = 0;
		while (i < lengthInt16(impl)) {
			var b = getInt16(impl, i);
			var size = getSequenceSize(b);
			if (pos == index) {
				return sub(impl, i, size, 1);
			}
			pos++;
			i += size;
		}
		return empty;
	}

	static function toCodeArray (impl:Utf16Impl) {
		var res = [];
		eachCode(impl, function (c) res.push(c));
		return res;
	}

	static inline function hasNoSurrogates (ba:Utf16Impl) {
		return lengthInt16(ba) == strLength(ba);
	}
	static inline function hasOnlySurrogates (ba:Utf16Impl) {
		return lengthInt16(ba) == (strLength(ba) << 1);
	}

	static function charCodeAt( ba:Utf16Impl, index : Int) : Null<Int> {
		if (index > 0 && index < strLength(ba)) {
			if (hasNoSurrogates(ba)) {
				return getCharCode(ba, index, 1);
			}
			if (hasOnlySurrogates(ba)) {
				return getCharCode(ba, index << 1, 2);
			}
		}
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < lengthInt16(ba)) {

			var b = getInt16(ba, i);
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

	static function fastCodeAt( ba:Utf16Impl, index : Int) : Null<Int> {
		if (index > 0 && index < strLength(ba)) {
			if (hasNoSurrogates(ba)) {
				return getCharCode(ba, index << 1, 2);
			}
			if (hasOnlySurrogates(ba)) {
				return getCharCode(ba, index << 2, 4);
			}
		}
		var pos = 0;
		var i = 0;
		while (i < lengthInt16(ba)) {
			var b = getInt16(ba, i);
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


	public static inline function eachCode ( impl:Utf16Impl, f : Int -> Void) {
		for (c in Utf16.fromImpl(impl)) f(c);
	}

	static function indexOf( impl:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int
	{
		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = lengthInt16(impl);
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getSequenceSize(getInt16(impl, i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getSequenceSize(getInt16(impl, i));
			var size2 = getSequenceSize(getInt16(str, j));

			if (size == size2 && compareChar(impl, i, str, j, size) == 0) {
				pos++;
				j+=size;
			} else {
				j = 0;
				pos = 0;
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				break;
			}
			if (pos == 0) {
				posFull++;
			}

		}
		return res;
	}

	static function lastIndexOf( ba:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;

		// byte iteration variables
		var i = 0;
		var j = 0;

		var iNext = 0;

		while (i < lengthInt16(ba) && (startIndexIsNull || posFull < startIndex + 1)) {
			var size = getSequenceSize(getInt16(ba, i));
			var size2 = getSequenceSize(getInt16(str, j));
			if (size == size2 && compareChar(ba, i, str, j, size) == 0) {
				if (j == 0) {
					// store the next position for next search
					iNext = i + size;
				}
				pos++;
				j+=size;

			} else {
				if (j > 0) {

					posFull++;
					i = iNext; // restore next search position and continue
					j = 0;
					pos = 0;
					continue;
				}
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				posFull++;
				i = iNext; // restore search position for next search
				j = 0;
				pos = 0;
				continue;
			}
			if (pos == 0) {
				posFull++;
			}
		}
		return res;
	}

	static function split( impl:Utf16Impl, delimiter : Utf16Impl ) : Array<Utf16>
	{
		var delimiterLen = strLength(delimiter);
		var buf = new StringBuf();
		var tmpBuf = new StringBuf();
		var bufLen = 0; // store utf8 len
		var tmpBufLen = 0; // store utf8 len

		var res:Array<Utf16> = [];

		var pos = 0;
		var posFull = 0;
		var i = 0;
		var j = 0;

		if (delimiter.length == 0) {
			while ( i < lengthInt16(impl)) {
				var size = getSequenceSize(getInt16(impl, i));
				res.push(Utf16.fromImpl(sub(impl, i, size, 1)));
				i+=size;
			}
			return res;
		}
		while (i < lengthInt16(impl)) {
			var size = getSequenceSize(getInt16(impl, i));
			var size2 = getSequenceSize(getInt16(delimiter, j));

			if (size == size2 && compareChar(impl, i, delimiter, j, size) == 0) {

				pos++;
				j+=size;
				tmpBufLen++;
				pushCharCode(impl, tmpBuf, i, size);
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addSub(tmpBuf.toString(),0);
					bufLen += tmpBufLen;
					tmpBufLen = 0;
					tmpBuf = new StringBuf();
				}
				pushCharCode(impl, buf, i, size);
				bufLen++;
			}
			i+=size;
			if (pos == delimiterLen) {
				if (buf.length > 0) {
					res.push(Utf16.fromImpl(mkImplFromBuffer(buf, bufLen)));
					bufLen = 0;
					buf = new StringBuf();
				} else {
					res.push(Utf16.fromImpl(empty));
				}
				tmpBuf = new StringBuf();
				tmpBufLen = 0;
				j = 0;
				pos = 0;
			}
			posFull++;
		}
		if (pos != 0) {
			j = 0;
			pos = 0;
			buf.addSub(tmpBuf.toString(),0);
			bufLen += tmpBufLen;
		}
		if (buf.length > 0) {
			res.push(Utf16.fromImpl(mkImplFromBuffer(buf, bufLen)));
		} else {
			res.push(Utf16.fromImpl(empty));
		}
		return res;
	}

	static function substr( str:Utf16Impl, pos : Int, ?len : Int ) : Utf16Impl {

		var lenIsNull = len == null;
		var byteLength = lengthInt16(str);
		if (pos < 0) {
			var thisLength = strLength(str);
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (!lenIsNull && len < 0) {
			len = strLength(str) + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return empty;

		var buf = new StringBuf();

		var cur = 0;

		var i = 0;
		var newSize = 0;
		while (i < byteLength) {
			var char = getInt16(str, i);
			var size = getSequenceSize(char);
			if (cur >= pos && (len == null || cur < pos + len))
			{
				newSize++;
				pushCharCode(str, buf, i, size);
			} else if (len != null && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return mkImplFromBuffer(buf, newSize);
	}

	static function substring( impl:Utf16Impl, startIndex : Int, ?endIndex : Int ) : Utf16Impl {
		var startIndex:Null<Int> = startIndex;
		var len = strLength(impl);
		var endIndexIsNull = endIndex == null;

		if (startIndex < 0) startIndex = 0;
		if (!endIndexIsNull && endIndex < 0) endIndex = 0;

		if (endIndexIsNull) endIndex = len;

 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;

		return if (hasNoSurrogates(impl)) {
			var len = (endIndex) - (startIndex);
			mkImpl(impl.s.substr( (startIndex), len), len);
		} else if (hasOnlySurrogates(impl)) {
			var len = (endIndex << 1)  - (startIndex << 1);
			mkImpl(impl.s.substr((startIndex << 1), len), len >> 1);
		} else {
			substr(impl, startIndex, endIndex - startIndex);
		}
	}

	static inline function fromCharCode( code : Int ) : Utf16Impl
	{

		return mkImpl(nativeStringfromCharCode(code), 1);
	}

	static inline function nativeStringfromCharCode( code : Int ) : String {
		return if (Convert.isSurrogatePair(code)) {
			#if hl
			String.fromCharCode(code);
			#else
			var surr = Convert.codePointToSurrogatePair(code);
			String.fromCharCode(surr.high) + String.fromCharCode(surr.low);
			#end
		} else {
			String.fromCharCode(code);
		}
	}

	static inline function compare (impl:Utf16Impl, other:Utf16Impl):Int {
		return impl.s < other.s ? -1 : impl.s > other.s ? 1 : 0;
	}
}

#else

import haxe.i18n.Tools;

@:structInit private class Utf16Impl {
	public var b(default, null) : ByteAccess;
	public var length(default,null) : Int;
}

class Utf16Iterator {
	var s:Utf16Impl;
	var p:Int;

	public inline function new (s:Utf16) {
		this.p = 0;
		this.s = s.impl();
	}
	public inline function hasNext ():Bool {
		return p < Utf16Tools.byteLength(s);
	}

	public inline function next ():Int {
		var b = Utf16Tools.getInt16(s, p);
		var size = Utf16Tools.getSequenceSize(b);
		var code = Utf16Tools.getCharCode(s, p, size);
		p += size;
		return code;
	}
}


@:allow(haxe.i18n)
abstract Utf16(Utf16Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = Utf16Tools.nativeStringToImpl(str);
	}

	public inline function iterator () {
		return new Utf16Iterator(fromImpl(this));
	}

	public inline function toUpperCase() : Utf16 {
		return fromImpl(Utf16Tools.toUpperCase(this));
	}
	public inline function toLowerCase() : Utf16 {
		return fromImpl(Utf16Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Utf16 {
		return fromImpl(Utf16Tools.charAt(this, index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf16Tools.charCodeAt(this, index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return Utf16Tools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf16 ) : Array<Utf16> {
		return Utf16Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substring(this, startIndex, endIndex));
	}

	public static inline function fromCharCode( code : Int ) : Utf16 {
		return fromImpl(Utf16Tools.fromCharCode(code));
	}

	@:op(A + B) inline function opAdd (other:Utf16):Utf16 {
		return fromImpl(Utf16Tools.append(this, other.impl()));
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return Utf16Tools.equal(this, other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	@:op(A > B) inline function opGreaterThan (other:Utf16) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf16) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf16) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf16) {
		return compare(other) >= 0;
	}

	// additional public api

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public inline function toNativeString() : String {
		return toUtf8().toNativeString();
		//return Utf16Tools.toNativeString(this);//this.getString(0, this.length);
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromUtf16(fromImpl(this));
	}

	public inline function toUtf8 ():Utf8 {
		return Utf8.fromUtf16(fromImpl(this));
	}


	public inline function toUtf32 ():Utf32 {
		return Utf32.fromUtf16(fromImpl(this));
	}


	public inline function toBytes() : haxe.io.Bytes {
		return Utf16Tools.toBytes(this);
	}

	public inline function toCodeArray () {
		return Utf16Tools.toCodeArray(this);
	}

	public static inline function fromByteAccess (ba:ByteAccess) {
		return fromImpl(Utf16Tools.fromByteAccess(ba));
	}

	public static inline function fromUtf8 (s:Utf8):Utf16 {
		return fromByteAccess(Convert.convertUtf8toUtf16(s.getReader(), true));
	}

	public static inline function fromUcs2 (s:Ucs2):Utf16 {
		#if (flash || js || hl || java || cs)
		return fromBytes(s.toBytes());
		#else
		// we can reuse the same underlying byteaccess because ucs2 allows supplementary chars
		return fromByteAccess(s.impl());
		#end
	}

	public static inline function fromUtf32 (s:Utf32):Utf16 {
		#if python
		return fromByteAccess(Tools.NativeStringTools.toUtf16ByteAccess(s.impl()));
		#else
		return fromByteAccess(Convert.convertUtf32toUtf16(s.getReader(), true));
		#end
	}

	public inline function getReader ():Utf16Reader
	{
		return new Utf16Reader(this.b);
	}

	public inline function eachCode ( f : Int -> Void) {
		return Utf16Tools.eachCode(this, f);
	}


	// private helpers

	inline function get_length() {
		return Utf16Tools.strLength(this);
	}

	static inline function fromImpl (impl:Utf16Impl):Utf16 {
		return cast impl;
	}

	inline function impl ():Utf16Impl {
		return this;
	}

	inline function compare (other:Utf16):Int {
		return Utf16Tools.compare(this, other.impl());
	}
}

abstract Utf16Reader(ByteAccess) {

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

	public inline function getInt16 (pos:Int) {
		return this.getInt16(pos);
	}
}

@:publicFields
private class Utf16Tools {

	static inline function fastGet (impl:Utf16Impl, pos:Int) {
		return impl.b.fastGet(pos);
	}

	static inline function set (impl:Utf16Impl, pos:Int, v:Int) {
		return impl.b.set(pos, v);
	}

	static inline function byteLength (ba:Utf16Impl) {
		return ba.b.length;
	}

	static inline function strLength(impl:Utf16Impl) {
		return impl.length;
	}

	static inline function mkImpl (b:ByteAccess, length:Int):Utf16Impl {
		return { b : b, length : length }
	}

	static inline function mkImplFromBuffer(buf:ByteAccessBuffer, newSize:Int):Utf16Impl {
		return mkImpl(buf.getByteAccess(), newSize);
	}

	static inline function sub (ba:Utf16Impl, pos:Int, size:Int, newLen:Int):Utf16Impl {
		var bytes = ba.b.sub(pos, size);
		return mkImpl(bytes, newLen);
	}

	static inline function nativeStringToImpl (s:String):Utf16Impl {
		if (s.length == 0) return empty;
		var ba = Tools.NativeStringTools.toUtf16ByteAccess(s);
		return mkImpl(ba, calcLength(ba));
	}

	static inline function allocImpl (size:Int, strLength:Int):Utf16Impl {
		return mkImpl(ByteAccess.alloc(size), strLength);
	}

	static inline function getInt16 (impl:Utf16Impl, pos:Int) {
		return impl.b.getInt16(pos);
	}

	static inline function getInt32 (impl:Utf16Impl, pos:Int) {
		return impl.b.getInt32(pos);
	}

	static inline function fromByteAccess (ba:ByteAccess):Utf16Impl {
		var len = calcLength(ba);
		return mkImpl(ba, len);
	}

	//public inline function toNativeString(impl:Utf16Impl) : String {
//
	//	//return impl.b.getString(0, impl.b.length);
	//}

	static inline function equal (impl:Utf16Impl, other:Utf16Impl) {
		return impl == other || (impl.length == other.length && impl.b.equal(other.b));
	}

	static inline function append (impl:Utf16Impl, other:Utf16Impl):Utf16Impl {
		if (other.length == 0) return impl;
		if (impl.length == 0) return other;
		return mkImpl(impl.b.append(other.b), impl.length + other.length);
	}

	static inline function toBytes(impl:Utf16Impl) : haxe.io.Bytes {
		return impl.b.copy().toBytes();
	}

	// helper functions

	static function calcLength(ba:ByteAccess) {
		var len = 0;
		var index = 0;
		while (index < ba.length) {
			var size = getSequenceSize(ba.getInt16(index));
			len++;
			index += size;
		}
		return len;
	}

	static inline function getSequenceSize (firstInt16:Int):Int {
		return if (Convert.isHighSurrogate(firstInt16)) 4 else 2;
	}

	static inline function isUpperCaseLetter (bytes:Utf16Impl, pos:Int, size:Int) {
		var b1 = fastGet(bytes, pos);
		var b2 = fastGet(bytes, pos+1);
		return b1 == 0x00 && b2 >= 0x41 && b2 <= 0x5A;
	}

	static inline function isLowerCaseLetter (bytes:Utf16Impl, pos:Int, size:Int) {
		var b1 = fastGet(bytes, pos);
		var b2 = fastGet(bytes, pos+1);
		return b1 == 0x00 && b2 >= 0x61 && b2 <= 0x7A;
	}

	static inline function toLowerCaseLetter (bytes:Utf16Impl, target:Utf16Impl, pos:Int, size:Int) {
		if (size == 2 && isUpperCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos));
			set(target, pos+1, fastGet(bytes, pos+1)+0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	static inline function toUpperCaseLetter (bytes:Utf16Impl, target:Utf16Impl, pos:Int, size:Int) {
		if (size == 2 && isLowerCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos));
			set(target, pos+1, fastGet(bytes, pos+1)-0x20);
		} else {
			for (i in 0...size) {
				set(target, pos + i, fastGet(bytes, pos+i));
			}
		}
	}

	static var empty = allocImpl(0, 0);

	static inline function getCharCode ( b:Utf16Impl, pos:Int, size:Int):Int {
		return switch size {
			case 2: getInt16(b, pos);
			case 4: Convert.surrogatePairToCharCode(getInt16(b, pos), getInt16(b, pos+2));
			case _: throw "invalid size, 2 or 4 expected";
		}
	}

	static inline function compareChar ( b1:Utf16Impl, pos1:Int, b2:Utf16Impl, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	static inline function pushCharCode (bytes:Utf16Impl, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(fastGet(bytes, pos+i));
		}
	}

	 // string functions

	static inline function map(impl:Utf16Impl, f:Utf16Impl->Utf16Impl->Int->Int->Void) : Utf16Impl {
		var res = allocImpl(byteLength(impl), strLength(impl));
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getSequenceSize(b);
			f(impl, res, i, size);
			i += size;
		}
		return res;
	}

	static inline function toUpperCase(impl:Utf16Impl) : Utf16Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return map(impl, function (impl, res, i, size) return toUpperCaseLetter(impl, res, i, size));
	}

	static inline function toLowerCase(impl:Utf16Impl) : Utf16Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return map(impl, function (impl, res, i, size) return toLowerCaseLetter(impl, res, i, size));
	}

	static function charAt(impl:Utf16Impl, index : Int) : Utf16Impl {
		if (index > 0 && index < strLength(impl)) {
			if (hasNoSurrogates(impl)) {
				return sub(impl, index << 1, 2, 1);
			}
			if (hasOnlySurrogates(impl)) {
				return sub(impl, index << 2, 2, 1);
			}
		}
		var pos = 0;
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getSequenceSize(b);
			if (pos == index) {
				return sub(impl, i, size, 1);
			}
			pos++;
			i += size;
		}
		return empty;
	}

	static function toCodeArray (impl:Utf16Impl) {
		var res = [];
		eachCode(impl, function (c) res.push(c));
		return res;
	}

	static inline function hasNoSurrogates (ba:Utf16Impl) {
		return byteLength(ba) == (strLength(ba) << 1);
	}
	static inline function hasOnlySurrogates (ba:Utf16Impl) {
		return byteLength(ba) == (strLength(ba) << 2);
	}

	static function charCodeAt( ba:Utf16Impl, index : Int) : Null<Int> {
		if (index > 0 && index < strLength(ba)) {
			if (hasNoSurrogates(ba)) {
				return getCharCode(ba, index << 1, 2);
			}
			if (hasOnlySurrogates(ba)) {
				return getCharCode(ba, index << 2, 4);
			}
		}
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < byteLength(ba)) {

			var b = getInt16(ba, i);
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

	static function fastCodeAt( ba:Utf16Impl, index : Int) : Null<Int> {
		if (index > 0 && index < strLength(ba)) {
			if (hasNoSurrogates(ba)) {
				return getCharCode(ba, index << 1, 2);
			}
			if (hasOnlySurrogates(ba)) {
				return getCharCode(ba, index << 2, 4);
			}
		}
		var pos = 0;
		var i = 0;
		while (i < byteLength(ba)) {
			var b = getInt16(ba, i);
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


	public static inline function eachCode ( impl:Utf16Impl, f : Int -> Void) {
		for (c in Utf16.fromImpl(impl)) f(c);
	}

	static function indexOf( impl:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int
	{
		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = byteLength(impl);
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getSequenceSize(getInt16(impl, i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getSequenceSize(getInt16(impl, i));
			var size2 = getSequenceSize(getInt16(str, j));

			if (size == size2 && compareChar(impl, i, str, j, size) == 0) {
				pos++;
				j+=size;
			} else {
				j = 0;
				pos = 0;
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				break;
			}
			if (pos == 0) {
				posFull++;
			}

		}
		return res;
	}

	static function lastIndexOf( ba:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;

		// byte iteration variables
		var i = 0;
		var j = 0;

		var iNext = 0;

		while (i < byteLength(ba) && (startIndexIsNull || posFull < startIndex + 1)) {
			var size = getSequenceSize(getInt16(ba, i));
			var size2 = getSequenceSize(getInt16(str, j));
			if (size == size2 && compareChar(ba, i, str, j, size) == 0) {
				if (j == 0) {
					// store the next position for next search
					iNext = i + size;
				}
				pos++;
				j+=size;

			} else {
				if (j > 0) {

					posFull++;
					i = iNext; // restore next search position and continue
					j = 0;
					pos = 0;
					continue;
				}
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				posFull++;
				i = iNext; // restore search position for next search
				j = 0;
				pos = 0;
				continue;
			}
			if (pos == 0) {
				posFull++;
			}
		}
		return res;
	}

	static function split( impl:Utf16Impl, delimiter : Utf16Impl ) : Array<Utf16>
	{
		var delimiterLen = strLength(delimiter);
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();
		var bufLen = 0; // store utf8 len
		var tmpBufLen = 0; // store utf8 len

		var res:Array<Utf16> = [];

		var pos = 0;
		var posFull = 0;
		var i = 0;
		var j = 0;

		if (delimiter.length == 0) {
			while ( i < byteLength(impl)) {
				var size = getSequenceSize(getInt16(impl, i));
				res.push(Utf16.fromImpl(sub(impl, i, size, 1)));
				i+=size;
			}
			return res;
		}
		while (i < byteLength(impl)) {
			var size = getSequenceSize(getInt16(impl, i));
			var size2 = getSequenceSize(getInt16(delimiter, j));

			if (size == size2 && compareChar(impl, i, delimiter, j, size) == 0) {

				pos++;
				j+=size;
				tmpBufLen++;
				for (k in 0...size) {
					tmpBuf.addByte(fastGet(impl,i+k));
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
					buf.addByte(fastGet(impl, i+k));
				}
				bufLen++;
			}
			i+=size;
			if (pos == delimiterLen) {
				if (buf.length > 0) {
					res.push(Utf16.fromImpl(mkImplFromBuffer(buf, bufLen)));
					bufLen = 0;
					buf = buf.reset();
				} else {
					res.push(Utf16.fromImpl(empty));
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
			res.push(Utf16.fromImpl(mkImplFromBuffer(buf, bufLen)));
		} else {
			res.push(Utf16.fromImpl(empty));
		}
		return res;
	}

	static function substr( str:Utf16Impl, pos : Int, ?len : Int ) : Utf16Impl {

		var lenIsNull = len == null;
		var byteLength = byteLength(str);
		if (pos < 0) {
			var thisLength = strLength(str);
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (!lenIsNull && len < 0) {
			len = strLength(str) + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return empty;

		var buf = new ByteAccessBuffer();

		var cur = 0;

		var i = 0;
		var newSize = 0;
		while (i < byteLength) {
			var char = getInt16(str, i);
			var size = getSequenceSize(char);
			if (cur >= pos && (len == null || cur < pos + len))
			{
				newSize++;
				pushCharCode(str, buf, i, size);
			} else if (len != null && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return mkImplFromBuffer(buf, newSize);
	}

	static function substring( impl:Utf16Impl, startIndex : Int, ?endIndex : Int ) : Utf16Impl {
		var startIndex:Null<Int> = startIndex;
		var len = strLength(impl);
		var endIndexIsNull = endIndex == null;

		if (startIndex < 0) startIndex = 0;
		if (!endIndexIsNull && endIndex < 0) endIndex = 0;

		if (endIndexIsNull) endIndex = len;

 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;

		return if (hasNoSurrogates(impl)) {
			var len = (endIndex << 1) - (startIndex << 1);
			mkImpl(impl.b.sub( (startIndex << 1), len), len >> 1);
		} else if (hasOnlySurrogates(impl)) {
			var len = (endIndex << 2)  - (startIndex << 2);
			mkImpl(impl.b.sub((startIndex << 2), len), len  >> 2);
		} else {
			substr(impl, startIndex, endIndex - startIndex);
		}
	}

	static inline function fromCharCode( code : Int ) : Utf16Impl
	{
		return mkImpl(Convert.codePointToUtf16ByteAccess(code), 1);
	}

	static inline function compare (impl:Utf16Impl, other:Utf16Impl):Int {
		return impl.b.compare(other.b);
	}
}
#end
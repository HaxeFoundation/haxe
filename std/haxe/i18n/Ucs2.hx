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

/**
	Cross platform UCS2 string API.
**/
#if (flash || js || hl || java || cs)


private class Ucs2Iterator {
	var s:Ucs2;
	var p:Int;

	public inline function new (s) {
		this.p = 0;
		this.s = s;
	}

	public inline function hasNext ():Bool {
		return p < s.length;
	}

	public inline function next ():Int {
		return s.fastCodeAt(p++);
	}
}

@:allow(haxe.i18n)
abstract Ucs2(String) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = str;
	}

	public inline function iterator () {
		return new Ucs2Iterator(fromImpl(this));
	}

	inline function get_length():Int {
		return this.length;
	}

	public inline function toUpperCase() : Ucs2 {
		return new Ucs2(this.toUpperCase());
	}

	public inline function toLowerCase() : Ucs2 {
		return new Ucs2(this.toLowerCase());
	}

	public inline function charAt(index : Int) : Ucs2 {
		return new Ucs2(this.charAt(index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return this.indexOf(str.impl(),startIndex);
	}

	public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.impl());
		}
		return this.lastIndexOf(str.impl(), startIndex);
	}

	public inline function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return cast this.split(delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		if (len == null) return new Ucs2(this.substr(pos)); // required for flash
		return new Ucs2(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		if (endIndex == null) return new Ucs2(this.substring(startIndex));
		return new Ucs2(this.substring(startIndex,endIndex));
	}

	public inline function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return new Ucs2(nativeStringfromCharCode(code));
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

	public function toBytes() : haxe.io.Bytes {
		var b = ByteAccess.alloc(length * 2);
		for (i in 0...length) {
			var code = fastCodeAt(i);
			b.setInt16LE(i*2, code);
		}
		return b.toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}


	public static function fromByteAccess(bytes:ByteAccess):Ucs2 {
		var i = 0;
		var buf = new StringBuf();
		while (i < bytes.length) {
			var code1 = bytes.getInt16LE(i);
			// we allow surrogates in ucs2, but we treat them as individual chars with invalid codes
			if (Convert.isHighSurrogate(code1)) {
				var code2 = bytes.getInt16LE(i+2);
				#if hl
				var code = Convert.surrogatePairToCharCode(code1, code2);
				StringBufTools.addString(buf, String.fromCharCode(code));
				#else
				// js, java, cs just allow String.fromCharCode for high and low surrogates
				StringBufTools.addString(buf, nativeStringfromCharCode(code1));
				StringBufTools.addString(buf, nativeStringfromCharCode(code2));
				#end
				i+=4;
			} else {
				StringBufTools.addString(buf, nativeStringfromCharCode(code1));
				i+=2;
			}
		}
		return new Ucs2(buf.toString());
	}

	@:op(A == B) inline function opEq (other:Ucs2) {
		return this == other.impl();
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return fromImpl(this + other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromUcs2(fromImpl(this));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromUcs2(fromImpl(this));
	}

	public function toUtf32() : Utf32 {
		return Utf32.fromUcs2( fromImpl(this) );
	}

	public static inline function fromUtf32 (s:Utf32):Ucs2 {
		var buf = new StringBuf();
		s.eachCode(code -> StringBufTools.addString(buf, nativeStringfromCharCode(code)));
		return fromImpl(buf.toString());
	}

	public static inline function fromUtf16 (s:Utf16):Ucs2 {
		return new Ucs2(s.toNativeString());
	}

	public static inline function fromUtf8(s:Utf8) : Ucs2 {
		var buf = new StringBuf();
		s.eachCode(code -> StringBufTools.addString(buf, nativeStringfromCharCode(code)));
		return fromImpl(buf.toString());
	}

	static inline function fromImpl (str:String):Ucs2 {
		return cast str;
	}



	inline function impl() : String {
		return this;
	}

	inline function eachCode ( f : Int -> Void) {
		for (c in fromImpl(this)) f(c);
	}

	public inline function getReader ():Ucs2Reader {
		return new Ucs2Reader(this);
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	@:op(A > B) inline function opGreaterThan (other:Ucs2) {
		return this > other.impl();
	}

	@:op(A < B) inline function opLessThan (other:Ucs2) {
		return this < other.impl();
	}

	@:op(A <= B) inline function opLessThanOrEq (other:Ucs2) {
		return this <= other.impl();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Ucs2) {
		return this >= other.impl();
	}

	public function isValid () {
		for (i in 0...length) {
			var code = fastCodeAt(i);
			if (Convert.isSurrogate(code)) return false;
		}
		return true;
	}
}

abstract Ucs2Reader(String) {
	public inline function new (s:String) {
		this = s;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length << 1;
	}

	public inline function getInt16 (pos:Int) {
		return StringTools.fastCodeAt(this, pos);
	}
}

#else

import haxe.i18n.ByteAccess;

private typedef Ucs2Impl = ByteAccess;

private class Ucs2Iterator {
	var s:Ucs2;
	var p:Int;

	public inline function new (s) {
		this.p = 0;
		this.s = s;
	}

	public inline function hasNext ():Bool {
		return p < s.length;
	}

	public inline function next ():Int {
		return Ucs2Tools.fastCodeAt(s.impl(), p++);
	}
}

@:allow(haxe.i18n)
abstract Ucs2(ByteAccess) {
	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = NativeStringTools.toUcs2ByteAccess(str);
	}

	public inline function iterator () {
		return new Ucs2Iterator(fromImpl(this));
	}

	public inline function getReader ():Ucs2Reader {
		return new Ucs2Reader(this);
	}

	inline function get_length():Int {
		return Ucs2Tools.strLength(this);
	}

	public static inline function asByteAccess( s:Ucs2 ) : ByteAccess {
		return cast s;
	}

	inline function eachCode ( f : Int -> Void) {
		Ucs2Tools.eachCode(this, f);
	}

	public function toCodeArray () {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	public inline function toUpperCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toUpperCase(this));
	}

	public inline function toLowerCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Ucs2 {
		return fromImpl(Ucs2Tools.charAt(this, index));
	}

	public inline function isValid ():Bool {
		return Ucs2Tools.isValid(this);
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Ucs2Tools.charCodeAt(this, index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return Ucs2Tools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public inline function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return Ucs2Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Ucs2 {
		return fromImpl(Ucs2Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		return fromImpl(Ucs2Tools.substring(this, startIndex, endIndex));
	}

	// private helpers
	static inline function fromByteAccess (bytes:ByteAccess):Ucs2 {
		return cast bytes;
	}

	static inline function fromImpl (impl:ByteAccess):Ucs2 {
		return cast impl;
	}

	inline function impl ():ByteAccess {
		return this;
	}

	public inline function toNativeString() : String {
		return Ucs2Tools.toNativeString(this);
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return fromImpl(Convert.codePointToUtf16ByteAccess(code));
	}

	public inline function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromImpl(ByteAccess.fromBytes(bytes).copy());
	}

	public static inline function fromUtf8(s:Utf8) : Ucs2 {
		return fromByteAccess(Convert.convertUtf8toUcs2(s.getReader(), true, false));
	}

	public static inline function fromUtf16 (s:Utf16):Ucs2 {
		return fromByteAccess(s.impl().b);
	}

	public static inline function fromUtf32 (s:Utf32):Ucs2 {
		return fromUtf16(Utf16.fromUtf32(s));
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromUcs2(fromImpl(this));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromUcs2(fromImpl(this));
	}

	public inline function toUtf32() : Utf32 {
		return Utf32.fromUcs2(fromImpl(this));
	}

	@:op(A == B) inline function opEq (other:Ucs2) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return fromImpl(this.append(asByteAccess(other)));
	}

	inline function compare (other:Ucs2):Int {
		return Ucs2Tools.compare(this, other.impl());
	}

	@:op(A > B) inline function opGreaterThan (other:Ucs2) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Ucs2) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Ucs2) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Ucs2) {
		return compare(other) >= 0;
	}
}

abstract Ucs2Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	public inline function getInt16 (pos:Int) {
		return this.getInt16LE(pos);
	}
}

@:publicFields
private class Ucs2Tools {

	static inline function strToImplIndex (strIndex:Int):Int {
		return strIndex << 1;
	}
	static inline function implToStrIndex (strIndex:Int):Int {
		return strIndex >> 1;
	}
	static inline function strLength(impl:Ucs2Impl):Int {
		return implToStrIndex(impl.length);
	}

	static inline function byteLength(impl:Ucs2Impl):Int {
		return impl.length;
	}

	static inline function isUpperCaseLetter (bytes:Int) {
		return bytes >= 0x41 && bytes <= 0x5A;
	}

	static inline function isLowerCaseLetter (bytes:Int) {
		return bytes >= 0x61 && bytes <= 0x7A;
	}

	static inline function toLowerCaseLetter (bytes:Int):Int {
		return if (isUpperCaseLetter(bytes)) {
			bytes + 0x20;
		} else {
			bytes;
		}
	}

	static inline function toUpperCaseLetter (bytes:Int) {
		return if (isLowerCaseLetter(bytes)) {
			bytes - 0x20;
		} else {
			bytes;
		}
	}

	static inline function map(impl:Ucs2Impl, f:Int->Int) : Ucs2Impl {
		var res = Ucs2Impl.alloc(byteLength(impl));
		var i = 0;
		while (i < impl.length) {
			var b = getInt16(impl, i);
			res.setInt16LE(i, f(b));
			i+=2;
		}
		return res;
	}

	static inline function toUpperCase(impl:Ucs2Impl) : Ucs2Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return map(impl, function (code) return toUpperCaseLetter(code));
	}

	static inline function toLowerCase(impl:Ucs2Impl) : Ucs2Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return map(impl, function (code) return toLowerCaseLetter(code));
	}

	static var empty = ByteAccess.alloc(0);

	static inline function getInt16 (impl:Ucs2Impl, pos:Int) {
		return impl.getInt16LE(pos);
	}

	static inline function charAt(impl:Ucs2Impl, index : Int) : Ucs2Impl {
		if (index < 0 || index >= strLength(impl)) {
			return empty;
		}
		var b = ByteAccess.alloc(2);
		b.setInt16LE(0, getInt16(impl, index*2));
		return b;
	}

	static function indexOf(impl:Ucs2Impl, str : Ucs2Impl, ?startIndex : Int ) : Int {
		var res = -1;
		var strLen = byteLength(str);
		var byteLen = byteLength(impl);

		var i = startIndex != null ? startIndex * 2 : 0;
		var pos = 0;
		var fullPos = i;
		while (i < byteLen) {
			if (getInt16(impl, i) == getInt16(str, pos)) {
				pos+=2;
			} else if (pos > 0) {
				pos = 0;
			}
			fullPos+=2;
			if (pos == strLen) {
				res = implToStrIndex(fullPos - strLen);
				break;
			}
			i+=2;
		}
		return res;
	}

	static function lastIndexOf( impl:Ucs2Impl, str : Ucs2Impl, ?startIndex : Int ) : Int {
		var byteLen = byteLength(str);
		var pos = byteLen-2;

		var startIndex = startIndex == null ? byteLength(impl) : strToImplIndex(startIndex) + byteLen;

		if (startIndex > byteLength(impl)) {
			startIndex = byteLength(impl);
		}
		var i = startIndex;
		var res = -1;
		var fullPos = startIndex;
		var lastPos = byteLen - 2;
		while (true) {
			i-=2;
			if (i <= -1) break;
			if (getInt16(impl, i) == getInt16(str, pos)) {
				pos-=2;
			} else if (pos < lastPos) {
				pos = lastPos;
			}
			fullPos-=2;
			if (pos == -2) {
				res = implToStrIndex(fullPos);
				break;
			}
		}
		return res;
	}

	static function substr( impl:Ucs2Impl, pos : Int, ?len : Int ) : Ucs2Impl {
		return if (len == null) {
			if (pos < 0) {
				var newPos = strLength(impl) + pos;
				if (newPos < 0) newPos = 0;
				substring(impl, newPos);
			} else {
				substring(impl, pos);
			}
		} else {
			if (len < 0) {
				substring(impl, pos, strLength(impl) + len);
			} else {
				substring(impl, pos, pos + len);
			}
		}
	}

	static function substring( impl:Ucs2Impl, startIndex : Int, ?endIndex : Int ) : Ucs2Impl {

		var startIndex:Null<Int> = startIndex; // make startIndex nullable, because of possible swap with endIndex

		if (startIndex < 0) startIndex = 0;
		if (endIndex != null && endIndex < 0) endIndex = 0;

		var len = strLength(impl);

		if (endIndex == null) endIndex = len;

 		if (startIndex > endIndex) { // swap startIndex and endIndex
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;

		return impl.sub(strToImplIndex(startIndex), strToImplIndex(endIndex) - strToImplIndex(startIndex) );
	}

	static function split( impl:Ucs2Impl, delimiter : Ucs2Impl ) : Array<Ucs2> {
		var delimiterLen = byteLength(delimiter);

		var lastPos = 0;
		var res = [];
		var pos = 0;

		var i = 0;
		if (delimiter.length == 0) {
			while ( i < byteLength(impl)) {
				res.push(Ucs2.fromImpl(impl.sub(i, 2)));
				i+=2;
			}
			return res;
		}

		while ( i < byteLength(impl)) {

			var b = getInt16(impl, i);
			var d = getInt16(delimiter, pos);

			if (b == d) {
				pos+=2;
			} else {
				pos = 0;
			}

			if (pos == delimiterLen) {
				var size = ((i+2) - delimiterLen) - lastPos;
				if (size > 0) {
					res.push(Ucs2.fromImpl(impl.sub(lastPos, size)));
				} else {
					res.push(Ucs2.fromImpl(empty));
				}
				pos = 0;
				lastPos = i+2;
			}
			i+=2;
		}

		if (lastPos < byteLength(impl)) {
			res.push(Ucs2.fromImpl(impl.sub(lastPos, byteLength(impl) - lastPos)));
		} else {
			res.push(Ucs2.fromImpl(empty));
		}

		return res;
	}

	static inline function charCodeAt( impl:Ucs2Impl, index : Int) : Null<Int> {
		if (index < 0 || index >= strLength(impl)) {
			return null;
		}
		return fastCodeAt(impl, index);
	}

	static inline function fastCodeAt( impl:Ucs2Impl, index : Int) : Int {
		return getInt16(impl, strToImplIndex(index));
		//return (impl.get(strToImplIndex(index)) << 8) | impl.get(strToImplIndex(index) + 1);
	}

	static inline function eachCode ( impl:Ucs2Impl, f : Int -> Void) {
		for (c in Ucs2.fromImpl(impl)) f(c);
	}

	static inline function toNativeString(impl:Ucs2Impl) : String {
		return Ucs2.fromImpl(impl).toUtf8().toNativeString();
	}

	static inline function compare (impl:Ucs2Impl, other:Ucs2Impl):Int {
		return impl.compare(other);
	}

	static function isValid(impl:Ucs2Impl) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			if (Convert.isSurrogate(code)) return false;
		}
		return true;
	}

}

#end
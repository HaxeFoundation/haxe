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

import haxe.i18n.Tools.NativeStringTools;

import haxe.i18n.Tools.Convert;

#if python

import haxe.i18n.Tools.StringBufTools;

@:allow(haxe.i18n)
abstract Utf32(String) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = str;
	}

	inline function get_length():Int {
		return this.length;
	}

	public inline function toUpperCase() : Utf32 {
		return new Utf32(this.toUpperCase());
	}

	public inline function toLowerCase() : Utf32 {
		return new Utf32(this.toLowerCase());
	}

	public inline function charAt(index : Int) : Utf32 {
		return new Utf32(this.charAt(index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf32, ?startIndex : Int ) : Int {
		return this.indexOf(str.impl(),startIndex);
	}

	public inline function lastIndexOf( str : Utf32, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.impl());
		}
		return this.lastIndexOf(str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf32 ) : Array<Utf32> {
		return cast this.split(delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Utf32 {
		if (len == null) return new Utf32(this.substr(pos)); // required for flash
		return new Utf32(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf32 {
		if (endIndex == null) return new Utf32(this.substring(startIndex));
		return new Utf32(this.substring(startIndex,endIndex));
	}

	public inline function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Utf32 {
		return new Utf32(nativeStringfromCharCode(code));
	}

	static inline function nativeStringfromCharCode( code : Int ) : String {
		return String.fromCharCode(code);
	}

	public function toBytes(  ) : haxe.io.Bytes {
		var res = haxe.io.Bytes.alloc(this.length << 2);
		for (i in 0...this.length) {
			res.setInt32(i << 2, fastCodeAt(i));
		}
		return res;
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf32 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromByteAccess (ba:ByteAccess):Utf32 {
		return new Utf32(ba.getData().decode("utf-32be"));
	}

	@:op(A == B) inline function opEq (other:Utf32) {
		return this == other.impl();
	}

	@:op(A + B) inline function opAdd (other:Utf32) {
		return fromImpl(this + other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf32) {
		return !opEq(other);
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromUtf32(fromImpl(this));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromUtf32(fromImpl(this));
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromUtf32(fromImpl(this));
	}

	inline static function fromUcs2 (s:Ucs2):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	inline static function fromUtf16 (s:Utf16):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	inline static function fromUtf8 (s:Utf8):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	static inline function fromImpl (str:String):Utf32 {
		return cast str;
	}

	inline function impl() : String {
		return this;
	}

	inline function eachCode ( f : Int -> Void) {
		for (i in 0...length) {
			var code = fastCodeAt(i);
			f(code);
		}
	}

	public inline function getReader ():Utf32Reader {
		return new Utf32Reader(this);
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	@:op(A > B) inline function opGreaterThan (other:Utf32) {
		return this > other.impl();
	}

	@:op(A < B) inline function opLessThan (other:Utf32) {
		return this < other.impl();
	}

	@:op(A <= B) inline function opLessThanOrEq (other:Utf32) {
		return this <= other.impl();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf32) {
		return this >= other.impl();
	}
}

abstract Utf32Reader(String) {
	public inline function new (s:String) {
		this = s;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	public inline function getInt32 (pos:Int) {
		return StringTools.fastCodeAt(this, pos);
	}
}

#else

import haxe.i18n.ByteAccess;

private typedef Utf32Impl = haxe.ds.Vector<Int>;

@:allow(haxe.i18n)
abstract Utf32(Utf32Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = NativeStringTools.toUtf32Vector(str);
	}

	public inline function getReader ():Utf32Reader {
		return new Utf32Reader(this);
	}

	inline function get_length():Int {
		return Utf32Tools.strLength(this);
	}

	public static inline function asImpl( s:Utf32 ) : Utf32Impl {
		return cast s;
	}

	inline function eachCode ( f : Int -> Void) {
		Utf32Tools.eachCode(this, f);
	}

	public function toCodeArray () {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	public inline function toUpperCase() : Utf32 {
		return fromImpl(Utf32Tools.toUpperCase(this));
	}

	public inline function toLowerCase() : Utf32 {
		return fromImpl(Utf32Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Utf32 {
		return fromImpl(Utf32Tools.charAt(this, index));
	}

	public inline function isValid ():Bool {
		return Utf32Tools.isValid(this);
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf32Tools.charCodeAt(this, index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return Utf32Tools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf32, ?startIndex : Int ) : Int {
		return Utf32Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf32, ?startIndex : Int ) : Int {
		return Utf32Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf32 ) : Array<Utf32> {
		return Utf32Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Utf32 {
		return fromImpl(Utf32Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf32 {
		return fromImpl(Utf32Tools.substring(this, startIndex, endIndex));
	}

	static function byteAccessToImpl (ba:ByteAccess) {
		var res = Utf32Tools.alloc(ba.length >> 2);
		var i = 0;
		while (i < ba.length) {
			res[i >> 2] = ba.getInt32(i);
			i+=4;
		}
		return res;
	}
	// private helpers
	static inline function fromByteAccess (bytes:ByteAccess):Utf32 {
		return fromImpl(byteAccessToImpl(bytes));
	}
	static inline function fromImpl (impl:Utf32Impl):Utf32 {
		return cast impl;
	}
	inline function impl ():Utf32Impl {
		return this;
	}
	// end private helpers

	public inline function toNativeString() : String {
		return Utf32Tools.toNativeString(this);
	}


	public static inline function fromCharCode( code : Int ) : Utf32 {
		var v = Utf32Tools.alloc(1);
		v[0] = code;
		return fromImpl(v);
	}

	public function toBytes(  ) : haxe.io.Bytes {
		var res = haxe.io.Bytes.alloc(this.length << 2);
		for (i in 0...this.length) {
			res.setInt32(i << 2, this[i]);
		}
		return res;
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf32 {
		var res = Utf32Tools.alloc(bytes.length >> 2);
		var i = 0;
		while (i < bytes.length) {
			res[i >> 2] = bytes.getInt32(i);
			i+=4;
		}
		return fromImpl(res);
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromUtf32(fromImpl(this));
	}


	public inline function toUtf16() : Utf16 {
		return Utf16.fromUtf32(fromImpl(this));
	}


	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromUtf32(fromImpl(this));
	}

	inline static function fromUcs2 (s:Ucs2):Utf32 {
		var r = Utf32Tools.alloc(s.length);
		var pos = 0;
		s.eachCode(function (codePoint) r[pos++] = codePoint);
		return fromImpl(r);
	}

	inline static function fromUtf8 (s:Utf8):Utf32 {
		var r = Utf32Tools.alloc(s.length);
		var pos = 0;
		s.eachCode(function (codePoint) r[pos++] = codePoint);
		return fromImpl(r);
	}


	inline static function fromUtf16 (s:Utf16):Utf32 {
		var r = Utf32Tools.alloc(s.length);
		var pos = 0;
		s.eachCode(function (codePoint) r[pos++] = codePoint);
		return fromImpl(r);
	}


	@:op(A == B) inline function opEq (other:Utf32) {
		return Utf32Tools.equal(this, other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf32) {
		return !opEq(other);
	}

	@:op(A + B) inline function opAdd (other:Utf32) {
		return fromImpl(Utf32Tools.append(this, other.impl()));
	}

	inline function compare (other:Utf32):Int {
		return Utf32Tools.compare(this, other.impl());
	}

	@:op(A > B) inline function opGreaterThan (other:Utf32) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf32) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf32) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf32) {
		return compare(other) >= 0;
	}
}

abstract Utf32Reader(Utf32Impl) {

	public inline function new (vector:Utf32Impl) {
		this = vector;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length << 2;
	}

	public inline function getInt32 (pos:Int) {
		return this[pos >> 2];
	}
}

@:publicFields
private class Utf32Tools {

	static inline function alloc (size:Int) {
		return new Utf32Impl(size);
	}

	static function append (v:Utf32Impl, other:Utf32Impl) {
		#if (python || js || php7 || php || flash)
		return Utf32Impl.fromData(v.toData().concat(other.toData()));
		#else
		var res = alloc(v.length + other.length);

		Utf32Impl.blit(v, 0, res, 0, v.length);
		Utf32Impl.blit(other, 0, res, v.length, other.length);

		return res;
		#end
	}

	static inline function strToImplIndex (strIndex:Int):Int {
		return strIndex;
	}
	static inline function implToStrIndex (strIndex:Int):Int {
		return strIndex;
	}

	static inline function strLength(impl:Utf32Impl):Int {
		return implToStrIndex(impl.length);
	}

	static inline function isUpperCaseLetter (code:Int) {
		return code >= 0x41 && code <= 0x5A;
	}

	static inline function isLowerCaseLetter (code:Int) {
		return code >= 0x61 && code <= 0x7A;
	}

	static inline function toLowerCaseLetter (code:Int):Int {
		return if (isUpperCaseLetter(code)) {
			code + 0x20;
		} else {
			code;
		}
	}

	static inline function toUpperCaseLetter (code:Int) {
		return if (isLowerCaseLetter(code)) {
			code - 0x20;
		} else {
			code;
		}
	}

	static inline function map(impl:Utf32Impl, f:Int->Int) : Utf32Impl {
		var res = alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl[i];
			res[i] = f(b);
			i++;
		}
		return res;
	}
	static function toUpperCase(impl:Utf32Impl) : Utf32Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return map(impl, function (code) return toUpperCaseLetter(code));
	}

	static function toLowerCase(impl:Utf32Impl) : Utf32Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return map(impl, function (code) return toLowerCaseLetter(code));
	}

	static var empty = alloc(0);

	static function charAt(impl:Utf32Impl, index : Int) : Utf32Impl {
		if (index < 0 || index >= strLength(impl)) {
			return empty;
		}
		var b = alloc(1);
		var pos = strToImplIndex(index);
		b[0] = impl[pos];
		return b;
	}

	static function indexOf(impl:Utf32Impl, str : Utf32Impl, ?startIndex : Int ) : Int {
		var res = -1;
		var strLen = str.length;

		var len = impl.length;
		var i = startIndex != null ? (strToImplIndex(startIndex)) : 0;
		var pos = 0;
		var fullPos = i;
		while (i < len) {
			if (impl[i] == str[pos]) {
				pos++;
			} else if (pos > 0) {
				pos = 0;
			}
			fullPos++;
			if (pos == strLen) {
				res = implToStrIndex(fullPos - strLen);
				break;
			}
			i++;
		}
		return res;
	}

	static function lastIndexOf( impl:Utf32Impl, str : Utf32Impl, ?startIndex : Int ) : Int {
		var len = str.length;
		var pos = len-1;

		var startIndex = startIndex == null ? impl.length : strToImplIndex(startIndex) + len;

		if (startIndex > impl.length) {
			startIndex = impl.length;
		}
		var i = startIndex;
		var res = -1;
		var fullPos = startIndex;
		var lastPos = len - 1;
		while (--i > -1) {
			if (impl[i] == str[pos]) {
				pos--;
			} else if (pos < lastPos) {
				pos = lastPos;
			}
			fullPos--;
			if (pos == -1) {
				res = implToStrIndex(fullPos);
				break;
			}
		}
		return res;
	}

	static function substr( impl:Utf32Impl, pos : Int, ?len : Int ) : Utf32Impl {
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

	static function substring( impl:Utf32Impl, startIndex : Int, ?endIndex : Int ) : Utf32Impl {
		var startIndex:Null<Int> = startIndex;
		if (startIndex < 0) startIndex = 0;
		if (endIndex != null && endIndex < 0) endIndex = 0;

		var len = strLength(impl);

		if (endIndex == null) endIndex = len;

 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;
		if (startIndex == 0 && endIndex == 0) return empty;
		return sub(impl, startIndex, endIndex-startIndex);
	}

	static inline function sub( impl:Utf32Impl, pos:Int, size:Int ) : Utf32Impl {
		var res = alloc(size);

		Utf32Impl.blit(impl, pos, res, 0, size);

		return res;
	}

	static function split( impl:Utf32Impl, delimiter : Utf32Impl ) : Array<Utf32> {
		var delimiterLen = delimiter.length;

		var lastPos = 0;
		var res = [];
		var pos = 0;

		var i = 0;

		if (delimiter.length == 0) {
			while ( i < impl.length) {
				res.push(Utf32.fromImpl(sub(impl, i,1)));
				i++;
			}
			return res;
		}

		while ( i < impl.length) {

			var b = impl[i];
			var d = delimiter[pos];

			if (b == d) {
				pos++;
			} else {
				pos = 0;
			}

			if (pos == delimiterLen) {
				var size = ((i+1) - delimiterLen) - lastPos;
				if ( size > 0) {

					res.push(Utf32.fromImpl(sub(impl, lastPos, size)));
				} else {
					res.push(Utf32.fromImpl(empty));
				}
				pos = 0;
				lastPos = i+1;
			}
			i++;
		}

		if (lastPos < impl.length) {
			res.push(Utf32.fromImpl(sub(impl, lastPos, impl.length - lastPos)));
		} else {
			res.push(Utf32.fromImpl(empty));
		}
		return res;
	}

	static inline function charCodeAt( impl:Utf32Impl, index : Int) : Null<Int> {
		if (index < 0 || index >= strLength(impl)) {
			return null;
		}
		return fastCodeAt(impl, index);
	}

	static inline function fastCodeAt( impl:Utf32Impl, index : Int) : Int {
		var pos = strToImplIndex(index);
		return (impl[pos]);
	}

	static inline function eachCode ( impl:Utf32Impl, f : Int -> Void) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			f(code);
		}
	}

	static inline function toNativeString(impl:Utf32Impl) : String {
		return Utf32.fromImpl(impl).toUtf8().toNativeString();
	}

	static function compare (impl:Utf32Impl, other:Utf32Impl):Int {
		var l1 = impl.length;
		var l2 = other.length;
		var min = l1 < l2 ? l1 : l2;
		for (i in 0...min) {
			var a = impl[i];
			var b = other[i];

			var res = a < b ? -1 : a > b ? 1 : 0;
			if (res != 0) return res;
		}

		return l1 < l2 ? -1 : l1 > l2 ? 1 : 0;
	}

	static function equal (impl:Utf32Impl, other:Utf32Impl):Bool {
		if (impl == other) return true;
		var l1 = impl.length;
		var l2 = other.length;
		if (l1 != l2) return false;

		for (i in 0...l1) {
			var a = impl[i];
			var b = other[i];

			if (a != b) return false;

		}
		return true;
	}

	static function isValid(impl:Utf32Impl) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			if (Convert.isHighSurrogate(code)) return false;
		}
		return true;
	}
}

#end
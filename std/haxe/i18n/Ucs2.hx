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

@:allow(haxe.i18n)
abstract Ucs2(String) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = str;
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

			var surr = Convert.codeToSurrogatePair(code);
			#if hl
			String.fromCharCode(code);
			#else
			String.fromCharCode(surr.high) + String.fromCharCode(surr.low);
			#end	
		} else {
			String.fromCharCode(code);
		}
	}

	public function toBytes() : haxe.io.Bytes {
		var b = haxe.io.Bytes.alloc(length*2);
		for (i in 0...length) {
			var code = fastCodeAt(i);
			b.set(i * 2, ((code & 0xFF00) >> 8));
			b.set(i * 2 + 1, (code & 0x00FF));
		}
		return b;
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromByteAccess(bytes:ByteAccess):Ucs2 {
		var i = 0;
		var res = "";
		while (i < bytes.length) {
			var code1 = (bytes.fastGet(i) << 8) | bytes.fastGet(i+1);
			// we allow surrogates in ucs2, but we treat them as individual chars with invalid codes
			if (Convert.isHighSurrogate(code1)) {
				var code2 = (bytes.fastGet(i+2) << 8) | bytes.fastGet(i+3);
				#if hl
				var code = Convert.utf16surrogatePairToCharCode(code1, code2); 
				res += String.fromCharCode(code);
				#else
				// js, java, cs just allow String.fromCharCode for high and low surrogates
				res += nativeStringfromCharCode(code1);
				res += nativeStringfromCharCode(code2);
				#end
				i+=4;
			} else {
				res += nativeStringfromCharCode(code1);
				i+=2;
			}
		}
		return new Ucs2(res);
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
		return Utf8.fromByteAccess(Convert.convertUcs2toUtf8(getReader(), StrictConversion));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromBytes(toBytes());
	}

	public inline function toUtf32() : Utf32 {
		return toUtf16().toUtf32();
	}

	static inline function fromImpl (str:String):Ucs2 {
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
			if (Convert.isHighSurrogate(code)) return false;
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

typedef Ucs2Impl = ByteAccess;

@:allow(haxe.i18n)
abstract Ucs2(ByteAccess) {
	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = NativeStringTools.toUcs2(str); 
	}

	public inline function getReader ():Ucs2Reader {
		return new Ucs2Reader(this);
	}

	inline function get_length():Int {
		return Ucs2Tools.strLength(this);
	}

	public static function asByteAccess( s:Ucs2 ) : ByteAccess {
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

	public function toUpperCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toUpperCase(this));
	}

	public function toLowerCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toLowerCase(this));
	}

	public function charAt(index : Int) : Ucs2 {
		return fromImpl(Ucs2Tools.charAt(this, index));
	}

	public function isValid ():Bool {
		return Ucs2Tools.isValid(this);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Ucs2Tools.charCodeAt(this, index);
	}

	public function fastCodeAt( index : Int) : Int {
		return Ucs2Tools.fastCodeAt(this, index);
	}



	public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return Ucs2Tools.split(this, delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		return fromImpl(Ucs2Tools.substr(this, pos, len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
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

	public function toNativeString() : String {
		return Ucs2Tools.toNativeString(this);
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return fromImpl(Convert.charCodeToUtf16ByteAccess(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromImpl(ByteAccess.fromBytes(bytes).copy());
	}

	public function toUtf8() : Utf8 {
		return toUtf16().toUtf8();
	}

	public function toUtf16() : Utf16 {
		// we can reuse the same underlying byteaccess because ucs2 allows supplementary chars
		return Utf16.fromByteAccess(this);
	}

	public function toUtf32() : Utf32 {
		return toUtf16().toUtf32();
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

	function compare (other:Ucs2):Int {
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
		return this.getInt16(pos);
	}
}

@:allow(haxe.i18n)
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

	static inline function isUpperCaseLetter (bytes:Int) {
		return bytes >= 0x0041 && bytes <= 0x005A;
	}

	static inline function isLowerCaseLetter (bytes:Int) {
		return bytes >= 0x0061 && bytes <= 0x007A;
	}

	static inline function toLowerCaseLetter (bytes:Int):Int {
		return if (isUpperCaseLetter(bytes)) {
			bytes + 0x0020;
		} else {
			bytes;
		}
	}

	static inline function toUpperCaseLetter (bytes:Int) {
		return if (isLowerCaseLetter(bytes)) {
			bytes - 0x0020;
		} else {
			bytes;
		}
	}

	static inline function map(impl:Ucs2Impl, f:Int->Int) : Ucs2Impl {
		var res = Ucs2Impl.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl.getInt16(i);
			res.setInt16(i, f(b));
			i+=2;
			
		}
		return res;
	}
	
	static function toUpperCase(impl:Ucs2Impl) : Ucs2Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return map(impl, function (code) return toUpperCaseLetter(code));
	}

	static function toLowerCase(impl:Ucs2Impl) : Ucs2Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return map(impl, function (code) return toLowerCaseLetter(code));
	}

	static var empty = ByteAccess.alloc(0); 

	public static function charAt(impl:Ucs2Impl, index : Int) : Ucs2Impl {
		if (index < 0 || index >= strLength(impl)) {
			return empty;
		}
		var b = ByteAccess.alloc(2);
		b.set(0, impl.get(index * 2));
		b.set(1, impl.get(index * 2 + 1));

		return b;
	}

	public static function indexOf(impl:Ucs2Impl, str : Ucs2Impl, ?startIndex : Int ) : Int {
		var res = -1;
		var strLen = str.length;

		var len = impl.length;
		var i = startIndex != null ? startIndex * 2 : 0;
		var pos = 0;
		var fullPos = i;
		while (i < len) {
			if (impl.fastGet(i) == str.fastGet(pos)) {
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

	public static function lastIndexOf( impl:Ucs2Impl, str : Ucs2Impl, ?startIndex : Int ) : Int {
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
			if (impl.fastGet(i) == str.fastGet(pos)) {
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

	public static function substr( impl:Ucs2Impl, pos : Int, ?len : Int ) : Ucs2Impl {
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

	public static function substring( impl:Ucs2Impl, startIndex : Int, ?endIndex : Int ) : Ucs2Impl {
		
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

	public static function split( impl:Ucs2Impl, delimiter : Ucs2Impl ) : Array<Ucs2> {
		var delimiterLen = delimiter.length;

		var lastPos = 0;
		var res = [];
		var pos = 0;
		
		var i = 0;
		if (delimiter.length == 0) {
			while ( i < impl.length) {
				res.push(Ucs2.fromImpl(impl.sub(i, 2)));
				i+=2;
			}
			return res;
		}

		while ( i < impl.length) {

			var b = impl.fastGet(i);
			var d = delimiter.fastGet(pos);

			if (b == d) {
				pos++;
			} else {
				pos = 0;
			}

			if (pos == delimiterLen) {
				var size = ((i+1) - delimiterLen) - lastPos;
				if (size > 0) {
					res.push(Ucs2.fromImpl(impl.sub(lastPos, size)));
				} else {
					res.push(Ucs2.fromImpl(empty));
				}
				pos = 0;
				lastPos = i+1;
			}
			i++;
		}

		if (lastPos < impl.length) {
			res.push(Ucs2.fromImpl(impl.sub(lastPos, impl.length - lastPos)));
		} else {
			res.push(Ucs2.fromImpl(empty));
		}
		
		return res;
	}

	public static function charCodeAt( impl:Ucs2Impl, index : Int) : Null<Int> {
		if (index < 0 || index >= strLength(impl)) {
			return null;
		}
		return fastCodeAt(impl, index);
	}

	static inline function fastCodeAt( impl:Ucs2Impl, index : Int) : Int {
		return (impl.get(strToImplIndex(index)) << 8) | impl.get(strToImplIndex(index) + 1);
	}

	static inline function eachCode ( impl:Ucs2Impl, f : Int -> Void) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			f(code);
		}
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
			if (Convert.isHighSurrogate(code)) return false;
		}
		return true;
	}
	
}

#end
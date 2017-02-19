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

import haxe.i18n.ByteAccess;

typedef Utf32Impl = ByteAccess;

@:allow(haxe.i18n)
abstract Utf32(Utf32Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = NativeStringTools.toUtf32(str); 
	}

	public inline function getReader ():Utf32Reader {
		return new Utf32Reader(this);
	}

	inline function get_length():Int {
		return Utf32Tools.strLength(this);
	}

	public static function asByteAccess( s:Utf32 ) : ByteAccess {
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

	public function isValid ():Bool {
		return Utf32Tools.isValid(this);
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf32Tools.charCodeAt(this, index);
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

	// private helpers
	static inline function fromByteAccess (bytes:ByteAccess):Utf32 {
		return cast bytes;
	}
	static inline function fromImpl (impl:ByteAccess):Utf32 {
		return cast impl;
	}
	inline function impl ():ByteAccess {
		return this;
	}
	// end private helpers

	public function toNativeString() : String {
		return Utf32Tools.toNativeString(this);
	}

	public static inline function fromCharCode( code : Int ) : Utf32 {
		return fromImpl(Convert.charCodeToUtf32ByteAccess(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf32 {
		return fromImpl(ByteAccess.fromBytes(bytes).copy());
	}
	
	public function toUtf8() : Utf8 {
		return Utf8.fromByteAccess(Convert.convertUtf32toUtf8(getReader(), StrictConversion));
	}
	
	public function toUtf16() : Utf16 {
		return Utf16.fromByteAccess(Convert.convertUtf32toUtf16(getReader(), StrictConversion));
	}

	public function toUcs2() : Ucs2 {
		return toUtf16().toUcs2();
	}

	@:op(A == B) inline function opEq (other:Utf32) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Utf32) {
		return !opEq(other);
	}

	@:op(A + B) inline function opAdd (other:Utf32) {
		return fromImpl(this.append(asByteAccess(other)));
	}

	function compare (other:Utf32):Int {
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

abstract Utf32Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}
	
	public inline function getInt32 (pos:Int) {
		return this.getInt32(pos);
	}
}

@:allow(haxe.i18n)
class Utf32Tools {

	static inline function strToImplIndex (strIndex:Int):Int {
		return strIndex << 2;
	}
	static inline function implToStrIndex (strIndex:Int):Int {
		return strIndex >> 2;
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

	static function toUpperCase(impl:Utf32Impl) : Utf32Impl {
		var res = Utf32Impl.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl.getInt32(i);
			res.setInt32(i, toUpperCaseLetter(b));
			i+=4;
			
		}
		return res;
	}

	static function toLowerCase(impl:Utf32Impl) : Utf32Impl {
		var res = Utf32Impl.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl.getInt32(i);
			res.setInt32(i, toLowerCaseLetter(b));
			i+=4;
		}

		return res;
	}

	static var empty = ByteAccess.alloc(0); 

	public static function charAt(impl:Utf32Impl, index : Int) : Utf32Impl {
		if (index < 0 || index >= strLength(impl)) {
			return empty;
		}
		var b = ByteAccess.alloc(4);
		var pos = strToImplIndex(index);
		b.set(0, impl.get(pos    ));
		b.set(1, impl.get(pos + 1));
		b.set(2, impl.get(pos + 2));
		b.set(3, impl.get(pos + 3));

		return b;
	}

	public static function indexOf(impl:Utf32Impl, str : Utf32Impl, ?startIndex : Int ) : Int {
		var res = -1;
		var strLen = str.length;

		var len = impl.length;
		var i = startIndex != null ? (strToImplIndex(startIndex)) : 0;
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

	public static function lastIndexOf( impl:Utf32Impl, str : Utf32Impl, ?startIndex : Int ) : Int {
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

	public static function substr( impl:Utf32Impl, pos : Int, ?len : Int ) : Utf32Impl {
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

	public static function substring( impl:Utf32Impl, startIndex : Int, ?endIndex : Int ) : Utf32Impl {
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
		
		return impl.sub(strToImplIndex(startIndex), strToImplIndex(endIndex) - strToImplIndex(startIndex));
	}

	public static function split( impl:Utf32Impl, delimiter : Utf32Impl ) : Array<Utf32> {
		var delimiterLen = delimiter.length;

		var lastPos = 0;
		var res = [];
		var pos = 0;

		var i = 0;

		if (delimiter.length == 0) {
			while ( i < impl.length) {
				res.push(Utf32.fromImpl(impl.sub(i, 4)));
				i+=4;
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
				if ( size > 0) {
					res.push(Utf32.fromImpl(impl.sub(lastPos, size)));
				} else {
					res.push(Utf32.fromImpl(empty));
				}
				pos = 0;
				lastPos = i+1;
			}
			i++;
		}

		if (lastPos < impl.length) {
			res.push(Utf32.fromImpl(impl.sub(lastPos, impl.length - lastPos)));
		} else {
			res.push(Utf32.fromImpl(empty));
		}
		return res;
	}

	public static function charCodeAt( impl:Utf32Impl, index : Int) : Null<Int> {
		if (index < 0 || index >= strLength(impl)) {
			return null;
		}
		return fastCodeAt(impl, index);
	}

	static inline function fastCodeAt( impl:Utf32Impl, index : Int) : Int {
		var pos = strToImplIndex(index);
		return (impl.get(pos) << 24) | (impl.get(pos+1) << 16) | (impl.get(pos+2) << 8) | impl.get(pos + 3);

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

	static inline function compare (impl:Utf32Impl, other:Utf32Impl):Int {
		return impl.compare(other);
	}
	
	static function isValid(impl:Utf32Impl) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			if (Convert.isHighSurrogate(code)) return false;
		}
		return true;
	}
}
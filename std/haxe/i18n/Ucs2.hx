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



/**
	Cross platform UCS2 string API.
**/
#if (flash || js)

@:allow(haxe.i18n)
abstract Ucs2(String) {

	@:extern public var length(get,never) : Int;

	@:extern public inline function new(str:String) : Void {
		// this implementation only allows platforms which have native UCS2 String.
		this = str;
	}

	@:extern inline function get_length():Int {
		return this.length;
	}

	/**
		Returns a Ucs2 where all characters of `this` Ucs2 are upper case.

		Affects the characters `a-z`. Other characters remain unchanged.
	**/
	@:extern public inline function toUpperCase() : Ucs2 {
		return new Ucs2(this.toUpperCase());
	}

	/**
		Returns a Ucs2 where all characters of `this` Ucs2 are lower case.

		Affects the characters `A-Z`. Other characters remain unchanged.
	**/
	@:extern public inline function toLowerCase() : Ucs2 {
		return new Ucs2(this.toLowerCase());
	}

	/**
		Returns the character at position `index` of `this` Ucs2.

		If `index` is negative or exceeds `this.length`, the empty Ucs2 ""
		is returned.
	**/
	@:extern public inline function charAt(index : Int) : Ucs2 {
		return new Ucs2(this.charAt(index));
	}

	/**
		Returns the character code at position `index` of `this` Ucs2.

		If `index` is negative or exceeds `this.length`, null is returned.

		To obtain the character code of a single character, "x".code can be used
		instead to @:extern public inline the character code at compile time. Note that this
		only works on Ucs2 literals of length 1.
	**/
	@:extern public inline function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	/**
		Returns the position of the leftmost occurence of `str` within `this`
		Ucs2.

		If `startIndex` is given, the search is performed within the substring
		of `this` Ucs2 starting from `startIndex`. Otherwise the search is
		performed within `this` Ucs2. In either case, the returned position
		is relative to the beginning of `this` Ucs2.

		If `str` cannot be found, -1 is returned.
	**/
	@:extern public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	/**
		Returns the position of the rightmost occurence of `str` within `this`
		Ucs2.

		If `startIndex` is given, the search is performed within the substring
		of `this` Ucs2 from 0 to `startIndex`. Otherwise the search is
		performed within `this` Ucs2. In either case, the returned position
		is relative to the beginning of `this` Ucs2.

		If `str` cannot be found, -1 is returned.
	**/
	@:extern public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return this.lastIndexOf(str.toNativeString(),startIndex);
	}

	/**
		Splits `this` Ucs2 at each occurence of `delimiter`.

		If `this` Ucs2 is the empty Ucs2 "", the result is not consistent
		across targets and may either be `[]` (on Js, Cpp) or `[""]`.

		If `delimiter` is the empty Ucs2 "", `this` Ucs2 is split into an
		Array of `this.length` elements, where the elements correspond to the
		characters of `this` Ucs2.

		If `delimiter` is not found within `this` Ucs2, the result is an Array
		with one element, which equals `this` Ucs2.

		If `delimiter` is null, the result is unspecified.

		Otherwise, `this` Ucs2 is split into parts at each occurence of
		`delimiter`. If `this` Ucs2 starts (or ends) with [delimiter}, the
		result Array contains a leading (or trailing) empty Ucs2 "" element.
		Two subsequent delimiters also result in an empty Ucs2 "" element.
	**/
	@:extern public inline function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return cast this.split(delimiter.toNativeString());
	}

	/**
		Returns `len` characters of `this` Ucs2, starting at position `pos`.

		If `len` is omitted, all characters from position `pos` to the end of
		`this` Ucs2 are included.

		If `pos` is negative, its value is calculated from the end of `this`
		Ucs2 by `this.length + pos`. If this yields a negative value, 0 is
		used instead.

		If the calculated position + `len` exceeds `this.length`, the characters
		from that position to the end of `this` Ucs2 are returned.

		If `len` is negative, the result is unspecified.
	**/
	@:extern public inline function substr( pos : Int, ?len : Int ) : Ucs2 {
		return new Ucs2(this.substr(pos,len));
	}

	/**
		Returns the part of `this` Ucs2 from `startIndex` to `endIndex`.

		If `startIndex` or `endIndex` are negative, 0 is used instead.

		If `startIndex` exceeds `endIndex`, they are swapped.

		If the (possibly swapped) `endIndex` is omitted or exceeds
		`this.length`, `this.length` is used instead.

		If the (possibly swapped) `startIndex` exceeds `this.length`, the empty
		Ucs2 "" is returned.
	**/
	@:extern public inline function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		return new Ucs2(this.substring(startIndex,endIndex));
	}

	/**
		Returns the native underlying String.
	**/
	@:extern public inline function toNativeString() : String {
		return this;
	}

	/**
		Returns the Ucs2 corresponding to the character code `code`.

		If `code` is negative or has another invalid value, the result is
		unspecified.
	**/
	@:extern public static inline function fromCharCode( code : Int ) : Ucs2 {
		return new Ucs2(String.fromCharCode(code));
	}

	@:extern public inline function toBytes(  ) : haxe.io.Bytes {
		var b = haxe.io.Bytes.alloc(length*2);
		for (i in 0...length) {
			var code = charCodeAt(i);
			if (code == null) throw "assert";
			b.set(i * 2, ((code & 0xFF00) >> 8));
			b.set(i * 2 + 1, (code & 0x00FF));
		}
		return b;
	}

	@:extern public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {

		var i = 0;
		var res = "";
		var bytes = ByteAccess.fromBytes(bytes);
		while (i < bytes.length) {
			var code = (bytes.fastGet(i) << 8) | bytes.fastGet(i+1);
			res += String.fromCharCode(code);
			i+=2;
		}
		return new Ucs2(res);
	}

	@:op(A == B) inline function opEq (other:Ucs2):Bool {
		return this == other.toNativeString();
	}

	@:op(A != B) inline function opNotEq (other:Ucs2):Bool {
		return !opEq(other);
	}

	@:extern public inline function toUtf8() : Utf8 {
		return EncodingTools.ucs2ToUtf8(new Ucs2(this));
	}

}
#else

import haxe.i18n.EncodingTools;
import haxe.i18n.ByteAccess;

@:allow(haxe.i18n)
abstract Ucs2(ByteAccess) {

	@:extern public var length(get,never) : Int;


	@:extern public inline function new(str:String) : Void {
		this = asByteAccess(EncodingTools.nativeStringToUcs2(str));
	}

	@:extern inline function get_length():Int {
		return this.length >> 1;
	}

	@:extern public inline function toUpperCase() : Ucs2 {
		var buffer = new ByteAccessBuffer();

		for ( i in 0...length) {
			buffer.addByte(this.fastGet(i));
		}
		return Ucs2.wrapAsUcs2(buffer.getByteAccess());
	}

	@:extern public inline function toLowerCase() : Ucs2 {
		var buffer = new ByteAccessBuffer();

		for ( i in 0...length) {
			buffer.addByte(this.fastGet(i));
		}
		return Ucs2.wrapAsUcs2(buffer.getByteAccess());
	}

	@:extern public inline function charAt(index : Int) : Ucs2 {
		var b = ByteAccess.alloc(2);
		b.set(0, this.get(index * 2));
		b.set(1, this.get(index * 2 + 1));

		return Ucs2.wrapAsUcs2(b);
	}

	@:extern public inline function charCodeAt( index : Int) : Null<Int> {
		return (this.get(index << 1) << 8) | this.get((index << 1) + 1);
	}

	@:extern public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		var res = -1;
		var str = asByteAccess(str);
		var strLen = str.length;
		var pos = 0;
		var len = this.length;
		for ( i in 0...len) {
			if (this.fastGet(i) == str.fastGet(pos)) {
				pos++;
			} else {
				pos = 0;
			}
			if (pos == strLen) {
				res = (pos - strLen) >> 1;
				break;
			}
		}
		return res;
	}

	@:extern public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		var str = asByteAccess(str);
		var len = str.length;
		var pos = len;
		var i = length;
		var res = -1;
		while (--i > -1) {
			if (this.fastGet(i) == str.fastGet(pos)) {
				pos++;
			} else {
				pos = len;
			}
			if (pos == len) {
				res = (pos - len) >> 1;
				break;
			}
		}
		return res;
	}

	@:extern public inline function split( delimiter : Ucs2 ) : Array<Ucs2> {
		var delimiter = asByteAccess(delimiter);
		var delimiterLen = delimiter.length;
		var buffer = new ByteAccessBuffer();
		var tempBuffer = new ByteAccessBuffer();

		var res = [];
		var pos = 0;

		for ( i in 0...this.length) {

			var b = this.fastGet(i);
			var d = delimiter.fastGet(pos);

			if (b == d) {
				tempBuffer.addByte(b);
				pos++;
			} else {
				if (pos > 0) {
					buffer.addBuffer(tempBuffer);
					tempBuffer.reset();
				}
				buffer.addByte(b);
				pos = 0;
			}

			if (pos == delimiterLen) {
				pos = 0;
				res.push(Ucs2.wrapAsUcs2(buffer.getByteAccess()));
				buffer.reset();
				tempBuffer.reset();
			}
		}

		if (pos > 0) {
			buffer.addBuffer(tempBuffer);
		}
		if (buffer.length > 0) {
			res.push(Ucs2.wrapAsUcs2(buffer.getByteAccess()));
		}
		return res;
	}

	@:extern public inline function substr( pos : Int, ?len : Int ) : Ucs2 {
		return if (len == null) {
			substring(pos);
		} else {
			substring(pos, pos + len);
		}
	}


	@:extern public inline function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		var b = this;

		if (endIndex == null) {
			endIndex = length-1;
		}

		if (startIndex < 0) startIndex = 0;
		if (endIndex < 0) endIndex = 0;


		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		return wrapAsUcs2(b.sub(startIndex * 2, endIndex * 2 - startIndex * 2));
	}


	@:extern public inline function toNativeString() : String {
		// Ucs2 to Utf8
		return EncodingTools.ucs2ToUtf8(wrapAsUcs2(this)).toNativeString();
	}

	@:extern public static inline function fromCharCode( code : Int ) : Ucs2 {
		return new Ucs2(String.fromCharCode(code));
	}


	@:extern public inline function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	@:extern public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return wrapAsUcs2(ByteAccess.fromBytes(bytes).copy());
	}

	@:extern public inline function toUtf8() : Utf8 {
		return EncodingTools.ucs2ToUtf8(wrapAsUcs2(this));
	}

	// operators

	@:op(A == B) @:extern inline function opEq (other:Ucs2) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) @:extern inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	// private helpers
	@:extern static inline function wrapAsUcs2 (bytes:ByteAccess):Ucs2 {
		return cast bytes;
	}

	@:extern static inline function asByteAccess( s:Ucs2 ) : ByteAccess {
		return cast s;
	}
}

#end
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

@:structInit class Utf16Impl {
	public var b(default, null) : ByteAccess;
	public var length(default,null) : Int;
}

@:allow(haxe.i18n)
@:access(haxe.i18n.Utf16Tools)
abstract Utf16(Utf16Impl) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = Utf16Tools.nativeStringToImpl(str);
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
		return Utf16Tools.toNativeString(this);//this.getString(0, this.length);
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromByteAccess(this.b);
	}

	public inline function toUtf8 ():Utf8 {
		return Utf8.fromByteAccess(Convert.convertUtf16toUtf8(getReader(), StrictConversion));
	}

	public inline function toUtf32 ():Utf32 {
		return Utf32.fromByteAccess(Convert.convertUtf16toUtf32(getReader(), StrictConversion));
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

	public inline function getReader ():Utf16Reader
	{
		return new Utf16Reader(this.b);
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

	static function strLength(impl:Utf16Impl) {
		return impl.length;
	}

	static inline function mkImplFromBuffer(buf:ByteAccessBuffer, newSize:Int):Utf16Impl {
		return {
			b : buf.getByteAccess(),
			length : newSize
		}
	}

	static inline function sub (ba:Utf16Impl, pos:Int, size:Int, newLen:Int):Utf16Impl {
		var bytes = ba.b.sub(pos, size);
		return {
			b : bytes,
			length : newLen
		}
	}

	static function nativeStringToImpl (s:String):Utf16Impl {
		if (s.length == 0) return empty;
		var ba = NativeStringTools.toUtf16(s);
		return {
			b : ba,
			length : calcLength(ba)
		}
	}

	static inline function allocImpl (size:Int, strLength:Int):Utf16Impl {
		return {
			b : ByteAccess.alloc(size),
			length : strLength
		}
	}

	static inline function getInt16 (impl:Utf16Impl, pos:Int) {
		return impl.b.getInt16(pos);
	}

	static inline function getInt32 (impl:Utf16Impl, pos:Int) {
		return impl.b.getInt32(pos);
	}

	static inline function fromByteAccess (ba:ByteAccess):Utf16Impl {
		var len = calcLength(ba);
		return { length : len, b : ba};
	}

	static function toNativeString(impl:Utf16Impl) : String {
		return impl.b.getString(0, impl.b.length);
	}

	static inline function equal (impl:Utf16Impl, other:Utf16Impl) {
		return impl.length == other.length && impl.b.equal(other.b);
	}

	static inline function append (impl:Utf16Impl, other:Utf16Impl):Utf16Impl {
		if (other.length == 0) return impl;
		if (impl.length == 0) return other;
		return {
			length : impl.length + other.length,
			b : impl.b.append(other.b)
		}
	}

	static inline function toBytes(impl:Utf16Impl) : haxe.io.Bytes {
		return impl.b.copy().toBytes();
	}

	// helper functions

	static function calcLength(ba:ByteAccess) {
		var len = 0;
		var index = 0;
		while (index < ba.length) {
			var size = getCharSize(ba.getInt16(index));
			len++;
			index += size;
		}
		return len;
	}

	static inline function getCharSize (firstInt16:Int):Int {
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
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	static var empty = allocImpl(0, 0);

	static inline function getCharCode ( b:Utf16Impl, pos:Int, size:Int):Int {
		return switch size {

			case 2: getInt16(b, pos);
			case 4: Convert.utf16surrogatePairToCharCode(getInt16(b, pos), getInt16(b, pos+2));
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
	
	static inline function modify(impl:Utf16Impl, f:Utf16Impl->Utf16Impl->Int->Int->Void) : Utf16Impl {
		var res = allocImpl(byteLength(impl), strLength(impl));
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getCharSize(b);
			f(impl, res, i, size);
			i += size;
		}
		return res;
	}

	static function toUpperCase(impl:Utf16Impl) : Utf16Impl {
		// directly using toUpperCaseLetter results in not inlined function
		return modify(impl, function (impl, res, i, size) return toUpperCaseLetter(impl, res, i, size));
	}

	static function toLowerCase(impl:Utf16Impl) : Utf16Impl {
		// directly using toLowerCaseLetter results in not inlined function
		return modify(impl, function (impl, res, i, size) return toLowerCaseLetter(impl, res, i, size));
	}

	static function charAt(impl:Utf16Impl, index : Int) : Utf16Impl {
		var pos = 0;
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getCharSize(b);
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

	static function charCodeAt( ba:Utf16Impl, index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < byteLength(ba)) {

			var b = getInt16(ba, i);
			var size = getCharSize(b);
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
		var pos = 0;
		var i = 0;
		while (i < byteLength(ba)) {
			var b = getInt16(ba, i);
			var size = getCharSize(b);
			if (pos == index) {
				return getCharCode(ba, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return 0;
	}


	static inline function eachCode ( impl:Utf16Impl, f : Int -> Void) {
		var i = 0;
		while (i < byteLength(impl)) {
			var b = getInt16(impl, i);
			var size = getCharSize(b);
			var code = getCharCode(impl, i, size);
			f(code);
			i += size;
		}
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
				var size = getCharSize(getInt16(impl, i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(getInt16(impl, i));
			var size2 = getCharSize(getInt16(str, j));

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
			var size = getCharSize(getInt16(ba, i));
			var size2 = getCharSize(getInt16(str, j));
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
				var size = getCharSize(getInt16(impl, i));
				res.push(Utf16.fromImpl(sub(impl, i, size, 1)));
				i+=size;
			}
			return res;
		}
		while (i < byteLength(impl)) {
			var size = getCharSize(getInt16(impl, i));
			var size2 = getCharSize(getInt16(delimiter, j));

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
					tmpBuf.reset();
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
					buf.reset();
				} else {
					res.push(Utf16.fromImpl(empty));
				}
				tmpBuf.reset();
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
			var size = getCharSize(char);
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
		
		return substr(impl, startIndex, endIndex - startIndex);
	}

	static function fromCharCode( code : Int ) : Utf16Impl
	{
		return {
			b : Convert.charCodeToUtf16ByteAccess(code),
			length : 1
		}
	}
	
	static inline function compare (impl:Utf16Impl, other:Utf16Impl):Int {
		return impl.b.compare(other.b);
	}
}
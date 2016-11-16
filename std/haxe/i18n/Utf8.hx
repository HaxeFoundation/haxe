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

/*
#if (lua)

@:allow(haxe.i18n)
abstract Utf8(String) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = str;
	}

	function get_length():Int {
		return this.length;
	}

	public function toUpperCase() : Utf8 {
		return new Utf8(this.toUpperCase());
	}

	public function toLowerCase() : Utf8 {
		return new Utf8(this.toLowerCase());
	}

	public function charAt(index : Int) : Utf8 {
		return new Utf8(this.charAt(index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public function indexOf( str : Utf8, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.toNativeString());
		}
		return this.lastIndexOf(str.toNativeString(), startIndex);
		
	}

	public function split( delimiter : Utf8 ) : Array<Utf8> {
		return cast this.split(delimiter.toNativeString());
	}

	public function substr( pos : Int, ?len : Int ) : Utf8 {
		if (len == null) return new Utf8(this.substr(pos)); // required for flash
		return new Utf8(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		if (endIndex == null) return new Utf8(this.substring(startIndex));
		return new Utf8(this.substring(startIndex,endIndex));
	}

	public function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Utf8 {
		return new Utf8(String.fromCharCode(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return throw "not implemented";
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {

		var i = 0;
		var res = "";
		var bytes = ByteAccess.fromBytes(bytes);
		while (i < bytes.length) {
			var code = (bytes.fastGet(i) << 8) | bytes.fastGet(i+1);
			res += String.fromCharCode(code);
			i+=2;
		}
		return new Utf8(res);
	}


	@:op(A == B) inline function opEq (other:Utf8) {
		return this == other.toNativeString();
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromNativeString(this + other.toNativeString());
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}

	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(new Utf8(this));
	}

	public static inline function fromByteAccess (bytes:ByteAccess):Utf8 {
		return throw "not implemented";
	}
	

	public function toUtf16() : Utf16 {
		return EncodingTools.utf8ToUtf16(new Utf8(this));
	}

	public static inline function fromNativeString (str:String):Utf8 {
		return new Utf8(str);
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		for (i in 0...length) {
			res.push(charCodeAt(i));
		}
		return res;
	}

	public static inline function asByteAccess (s:Utf8):ByteAccess {
		return ByteAccess.fromBytes(s.toBytes());
	}

	@:op(A > B) inline function opGreaterThan (other:Utf8) {
		return this > other.toNativeString();
	}
	@:op(A < B) inline function opLessThan (other:Utf8) {
		return this < other.toNativeString();
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf8) {
		return this <= other.toNativeString();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf8) {
		return this >= other.toNativeString();
	}


}
#else
*/
import haxe.io.Bytes;

/**
	Cross platform Utf8 string API.
**/

/*
typedef Utf8ImplData = {
	bytes : ByteAccess,
	stringLength : Int
}

// impl has o(1) access to utf8 length
abstract Utf8Impl(Utf8ImplData) {
	inline function new (ba:ByteAccess, stringLength:Int) {
		this = {
			bytes : ba,
			stringLength : stringLength
		}
	}

	inline function fastGet (i) {
		return this.bytes.fastGet(i);
	}

	inline function byteLength (i) {
		return this.bytes.length;
	}

	public function toBytes() : haxe.io.Bytes {
		return this.bytes.copy().toBytes();
	}

	public function append(other:Utf8Impl) : Utf8Impl {
		var otherImpl = other.impl();
		var newBytes = this.bytes.append(otherImpl.bytes);
		return new Utf8Impl(newBytes, this.stringLength + otherImpl.stringLength);
	}

	inline function sub (i, size, newStringLength) {
		return new Utf8Impl(this.bytes.sub(i,size), newStringLength);
	}

	inline function impl () {
		return this;
	}
}
*/


@:allow(haxe.i18n)
abstract Utf8(ByteAccess) {

	public var length(get,never) : Int;
	
	public function new(str:String) : Void {
		this = asByteAccess(Utf8.fromNativeString(str));
	}
	
	public function toUpperCase() : Utf8 {
		var ba = Helper.toUpperCase(this);
		return fromByteAccessAndSize(ba, length);
	}
	
	public function toLowerCase() : Utf8 {
		var ba = Helper.toLowerCase(this);
		return fromByteAccessAndSize(ba, length);
	}
	
	public function charAt(index : Int) : Utf8 {
		var ba = Helper.charAt(this, index);
		return fromByteAccessAndSize(ba, 1);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Helper.charCodeAt(this, index);
	}
	
	public function indexOf( str : Utf8, ?startIndex : Int ) : Int
	{
		 return Helper.indexOf(this, asByteAccess(str), str.length, startIndex);
	}

	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		return Helper.lastIndexOf(this, asByteAccess(str), str.length, startIndex);
	}

	public function split( delimiter : Utf8 ) : Array<Utf8>
	{
		return Helper.split(this, asByteAccess(delimiter), delimiter.length, function (b,s) return fromByteAccessAndSize(b,s));
	}
	
	@:analyzer(no_code_motion) public function substr( pos : Int, ?len : Int ) : Utf8 {

		return Helper.substr(this, pos, function (b,s) return fromByteAccessAndSize(b,s), len);
	}
	
	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		return Helper.substring(this, length, startIndex, function (b,s) return fromByteAccessAndSize(b,s), endIndex);
	}
	
	public static function fromCharCode( code : Int ) : Utf8
	{
		var ba = Helper.fromCharCode(code);
		return fromByteAccessAndSize(ba, 1);
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromByteAccess(this.append(asByteAccess(other)));
	}

	@:op(A == B) public function opEq (other:Utf8) {
		return this.equal(asByteAccess(other));
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

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}

	public function toNativeString() : String {
		return this.getString(0, this.length);
	}
 	
	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(fromByteAccess(this));
	}
	
 	public function toUtf16 ():Utf16 {
		return EncodingTools.utf8ToUtf16(fromByteAccess(this));
	}
	
	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public function toCodeArray ():Array<Int> {
		return Helper.toCodeArray(this);
	}

	// private api

	static inline function fromByteAccessAndSize (ba:ByteAccess, size:Int) {
		return fromByteAccess(ba);
	}

	function get_length() {
		 return Helper.getLength(this);
	}

	static function empty () {
		return new Utf8("");
	}
	
	static function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}
	
	static inline function fromByteAccess (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}
	
	static function fromNativeString (s:String):Utf8 {
		return fromByteAccess(Helper.nativeStringToByteAccess(s));
 	}

	function compare (other:Utf8):Int {
		return Helper.compare(this, length, asByteAccess(other), other.length);
	}

	
}


@:allow(haxe.i18n)
class Helper {
	static function getLength(ba:ByteAccess) {
		var len = 0;
		var index = 0;
		while (index < ba.length) {
			var size = getCharSize(ba.fastGet(index));
			len++;
			index += size;
		}
		return len;
	}

	static function getCharSize (start:Int):Int {
		return if (start < 0x80) 1
		else if ((start & 0xE0) == 0xE0) 3
		else if ((start & 0xC0) == 0xC0) 2
		else throw "invalid utf8";
	}

	
	static function toUpperCase(ba:ByteAccess) : ByteAccess {
		var res = ByteAccess.alloc(ba.length);
		var i = 0;
		while (i < ba.length) {
			var b = ba.fastGet(i);
			var size = getCharSize(b);
			toUpperCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}


	
	static function isUpperCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = bytes.fastGet(pos);
		return b >= 0x41 && b <= 0x5A;
	}

	
	static function isLowerCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = bytes.fastGet(pos);
		return b >= 0x61 && b <= 0x7A;
	}

	
	static function toLowerCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos)+0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	
	static function toUpperCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos)-0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	
	static function toLowerCase(ba:ByteAccess) : ByteAccess {
		var res = ByteAccess.alloc(ba.length);
		var i = 0;
		while (i < ba.length) {
			var b = ba.fastGet(i);
			var size = getCharSize(b);
			toLowerCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}

	
	static function charAt(ba:ByteAccess, index : Int) : ByteAccess {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < ba.length) {
			var b = ba.fastGet(i);
			var size = getCharSize(b);
			if (pos == index) {
				res = ba.sub(i, size);
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty() : res;
	}

	static var emptyByteAccess = ByteAccess.alloc(0);

	static function empty () {
		return emptyByteAccess;
	}

	
	static inline function eachCode ( ba:ByteAccess, f : Int -> Void) {
		var i = 0;
		while (i < ba.length) {
			var b = ba.fastGet(i);
			var size = getCharSize(b);
			var code = getCharCode(ba, i, size);
			f(code);
			i += size;
		}
	}


	static function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {
			case 1: b.fastGet(pos);
			case 2: (b.fastGet(pos) << 8) | (b.fastGet(pos+1));
			case 3: (b.fastGet(pos) << 16) | (b.fastGet(pos+1) << 8) | b.fastGet(pos+2);
			case _: throw "invalid byte sequence";
		}
	}
	
	static function charCodeAt( ba:ByteAccess, index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < ba.length) {

			var b = ba.fastGet(i);
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

	
	static function indexOf( ba:ByteAccess, str : ByteAccess, strLen:Int, ?startIndex : Int ) : Int
	{

		var res = -1;
		var len = strLen; // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = ba.length;
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getCharSize(ba.fastGet(i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(ba.fastGet(i));

			if (compareChar(ba, i, str, j, size) == 0) {
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


	static function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}
	

	@:analyzer(no_code_motion) static function lastIndexOf( ba:ByteAccess, str : ByteAccess, strLen:Int, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var other = str;
		var res = -1;
		var len = strLen; // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = ba.length;
		
		// byte iteration variables
		var i = 0;
		var j = 0;
		
		var iNext = 0;
		
		while (i < byteLength && (startIndexIsNull || posFull < startIndex + 1)) {
			var size = getCharSize(ba.fastGet(i));
			if (compareChar(ba, i, str, j, size) == 0) {
				if (j == 0) {
					// store the next position for next search
					iNext = i + size;
				}
				pos++;
				j+=size;
				
			} else {
				j = 0;
				pos = 0;
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

	static inline function split<T>( ba:ByteAccess, delimiter : ByteAccess, delimiterLen:Int, mkElement:ByteAccess->Int->T ) : Array<T>
	{
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();

		var res:Array<T> = [];
		var len = delimiter.length; // O(n)
		var str = delimiter;
		var pos = 0;
		var posFull = 0;
		var byteLength = ba.length;
		// byte iteration variables
		var i = 0;
		var j = 0;
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(ba.fastGet(i));

			if (compareChar(ba, i, str, j, size) == 0) {

				pos++;
				j+=size;
				for (k in 0...size) {
					tmpBuf.addByte(ba.fastGet(i+k));
				}
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addBuffer(tmpBuf);
					tmpBuf.reset();
				}
				for (k in 0...size) {
					buf.addByte(ba.fastGet(i+k));
				}
			}
			i+=size;
			if (pos == len) {
				if (buf.length > 0) {
					var ba = buf.getByteAccess();
					var len = Helper.getLength(ba);
					res.push(mkElement(ba, len));
					buf.reset();
				} else {
					res.push(mkElement(empty(), 0));
				}
				tmpBuf.reset();
				j = 0;
				pos = 0;
			}
			posFull++;
		}
		if (pos != 0) {
			j = 0;
			pos = 0;
			buf.addBuffer(tmpBuf);
		}
		if (buf.length > 0) {
			var ba = buf.getByteAccess();
			var len = Helper.getLength(ba);
			res.push(mkElement(ba, len));
		} else {
			res.push(mkElement(empty(), 0));
		}
		return res;
	}
	
	@:analyzer(no_code_motion) static inline function substr<T>( ba:ByteAccess, pos : Int, mkElement:ByteAccess->Int->T, ?len : Int ) : T {

		var lenIsNull = len == null;
		if (pos < 0) {
			var thisLength = ba.length;
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (!lenIsNull && len < 0) {
			len = ba.length + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return mkElement(empty(), 0);

		var buf = new ByteAccessBuffer();

		var str = ba;

		var cur = 0;

		var byteLength = str.length;
		var i = 0;
		var newSize = 0;
		while (i < byteLength) {
			var char = str.fastGet(i);
			//if (char == null) throw "error";
			var size = getCharSize(char);
			if (cur >= pos && (lenIsNull || cur < pos + len))
			{
				newSize++;
				pushCharCode(str, buf, i, size);
			} else if (!lenIsNull && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return mkElement(buf.getByteAccess(), newSize);
	}

	
	static function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(bytes.fastGet(pos+i));
		}
	}

	
	
	@:analyzer(no_code_motion) static inline function substring<T>( ba:ByteAccess, len:Int, startIndex : Int, mkElement:ByteAccess->Int->T, ?endIndex : Int ) : T {
		var endIndexIsNull = endIndex == null; 
		var startIndex:Null<Int> = startIndex;
		if (startIndex < 0) startIndex = 0;
		if (!endIndexIsNull && endIndex < 0) endIndex = 0;
		
		if (endIndexIsNull) endIndex = len;
 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return mkElement(empty(), 0);
		
		return substr(ba, startIndex, mkElement, endIndex - startIndex);
	}


	static function fromCharCode( code : Int ) : ByteAccess
	{
		var size = getCodeSize(code);
		var bytes = ByteAccess.alloc(size);
		switch size {
			case 1:
				bytes.set(0, code);
			case 2:
				bytes.set(0, 0xC0 | (code >> 6));
				bytes.set(1, 0x80 | (code & 0x3F));
			case 3:
				bytes.set(0, 0xE0 | (code >> 12));
				bytes.set(1, 0x80 | ((code >> 6) & 0x3F));
				bytes.set(2, 0x80 | (code & 0x3F));
			case 4:
				bytes.set(0, 0xF0 | (code >> 18));
				bytes.set(1, 0x80 | ((code >> 12)  & 0x3F));
				bytes.set(2, 0x80 | ((code >> 6) & 0x3F));
				bytes.set(3, 0x80 | (code & 0x3F));
			case _: throw "invalid char code";
		}
		return bytes;
	}


	static function toCodeArray (ba:ByteAccess):Array<Int> {
		var res = [];
		eachCode(ba, function (c) res.push(c));
		return res;
	}


	static function getCodeSize (code:Int):Int {
		return if (code <= 0x7F) {
			1;
		} else if (code <= 0x7FF) {
			2;
		} else if (code <= 0xFFFF) {
			3;
		} else if (code <= 0x10FFFF) {
			4;
		} else {
			throw "invalid code " + code;
		}
	}


	static function nativeStringToByteAccess (s:String):ByteAccess {

 		#if python
 		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8"));
 		#elseif (js || flash)
		return EncodingTools.ucs2ToUtf8ByteAccess( new Ucs2(s));
 		#else
 		return ByteAccess.fromBytes(Bytes.ofString(s));
 		#end
 	}

	 static function compare (ba:ByteAccess, baLen:Int, other:ByteAccess, otherLen:Int):Int {
		var len1 = baLen;
		var len2 = otherLen;
		var min = Std.int(Math.min(len1, len2));
		for (i in 0...min) {
			var a = charCodeAt(ba, i);
			var b = charCodeAt(other, i);
			if (a < b) return -1;
			if (a > b) return 1;
		}
		if (len1 < len2) return -1;
		if (len1 > len2) return 1;
		return 0;
	}

	/*
	
	inline function getByteLength() {
		return this.length;
	}

	// private helpers

	
	public static function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}

	
	public static function fromByteAccess (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}

	

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}


	
	public static function fromNativeString (s:String):Utf8 {
		return fromByteAccess(nativeStringToByteAccess(s));
 	}

 	
	public function toNativeString() : String {
		return this.getString(0, this.length);
	}

 	
	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(fromByteAccess(this));
	}

	
 	public function toUtf16 ():Utf16 {
		return EncodingTools.utf8ToUtf16(fromByteAccess(this));
	}

	
	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromByteAccess(this.append(asByteAccess(other)));
	}

	@:op(A == B) public function opEq (other:Utf8) {
		return this.equal(asByteAccess(other));
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
	*/
}

//#end
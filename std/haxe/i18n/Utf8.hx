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
@:allow(haxe.i18n)
abstract Utf8(ByteAccess) {

	public var length(get,never) : Int;
	
	public function new(str:String) : Void {
		this = asByteAccess(Utf8.fromNativeString(str));
	}

	
	public function toUpperCase() : Utf8 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			toUpperCaseLetter(this, res, i, size);

			i += size;
		}
		return fromByteAccess(res);
	}

	
	public function toLowerCase() : Utf8 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			toLowerCaseLetter(this, res, i, size);

			i += size;
		}
		return fromByteAccess(res);
	}

	
	public function charAt(index : Int) : Utf8 {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			if (pos == index) {
				res = Utf8.fromByteAccess(this.sub(i, size));
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty() : res;
	}

	
	public function charCodeAt( index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < this.length) {

			var b = this.fastGet(i);
			var size = getCharSize(b);
			if (pos == index) {
				r = getCharCode(this, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return r;
	}

	
	public function indexOf( str : Utf8, ?startIndex : Int ) : Int
	{

		var res = -1;
		var len = str.length; // O(n)
		var str = asByteAccess(str);
		var pos = 0;
		var posFull = 0;
		var byteLength = this.length;
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getCharSize(this.fastGet(i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(this.fastGet(i));

			if (compareChar(this, i, str, j, size) == 0) {
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

	public inline function reverse ():Utf8 {
		var byteLen = this.length;
		var res = ByteAccess.alloc(byteLen);
		var i = 0;

		while (i < byteLen) {
			var b = this.fastGet(i);
			var size = getCharSize(b);

			for (s in 0...size) {
				res.set(byteLen - i - size + s, this.fastGet(i+s));
			}
			i += size;
		}
		return fromByteAccess(res);
	}

	
	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		var rev = fromByteAccess(this).reverse();
		var strRev = str.reverse();
		var thisLength = fromByteAccess(this).length;
		var sIndex = startIndex != null ? (thisLength - 1) - startIndex - (strRev.length - 1) : null;
		var i = rev.indexOf(strRev, sIndex);
		var res = i > -1 ? ((thisLength - 1) - (i + strRev.length - 1)) : i;
		return res;

	}

	
	public function split( delimiter : Utf8 ) : Array<Utf8>
	{
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();

		var res:Array<Utf8> = [];
		var len = delimiter.length; // O(n)
		var str = asByteAccess(delimiter);
		var pos = 0;
		var posFull = 0;
		var byteLength = this.length;
		// byte iteration variables
		var i = 0;
		var j = 0;
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(this.fastGet(i));

			if (compareChar(this, i, str, j, size) == 0) {

				pos++;
				j+=size;
				for (k in 0...size) {
					tmpBuf.addByte(this.fastGet(i+k));
				}
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addBuffer(tmpBuf);
					tmpBuf.reset();
				}
				for (k in 0...size) {
					buf.addByte(this.fastGet(i+k));
				}
			}
			i+=size;
			if (pos == len) {
				if (buf.length > 0) {
					res.push(fromByteAccess(buf.getByteAccess()));
					buf.reset();
				} else {
					res.push(empty());
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
			res.push(fromByteAccess(buf.getByteAccess()));
		} else {
			res.push(empty());
		}
		return res;
	}
	
	public function substr( pos : Int, ?len : Int ) : Utf8 {


		if (pos < 0) {
			var thisLength = fromByteAccess(this).length;
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (len != null && len < 0) {
			len = fromByteAccess(this).length + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return empty();

		var buf = new ByteAccessBuffer();

		var str = this;

		var cur = 0;

		var byteLength = str.length;
		var i = 0;

		while (i < byteLength) {
			var char = str.fastGet(i);
			//if (char == null) throw "error";
			var size = getCharSize(char);
			if (cur >= pos && (len == null || cur < pos + len))
			{
				pushCharCode(str, buf, i, size);
			} else if (len != null && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return fromByteAccess(buf.getByteAccess());
	}

	
	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		var startIndex:Null<Int> = startIndex;
		if (startIndex < 0) startIndex = 0;
		if (endIndex != null && endIndex < 0) endIndex = 0;
		
		var len = fromByteAccess(this).length;
		if (endIndex == null) endIndex = len;
 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return new Utf8("");
		
		return substr(startIndex, endIndex - startIndex);
	}

	

	// private helpers

	 function get_length() {
		var len = 0;
		var index = 0;
		while (index < this.length) {
			var size = getCharSize(this.fastGet(index));
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

	
	static function empty () {
		return new Utf8("");
	}

	
	static function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {
			case 1: b.fastGet(pos);
			case 2: (b.fastGet(pos) << 8) | (b.fastGet(pos+1));
			case 3: (b.fastGet(pos) << 16) | (b.fastGet(pos+1) << 8) | b.fastGet(pos+2);
			case _: throw "invalid byte sequence";
		}
	}

	
	static function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	
	static function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(bytes.fastGet(pos+i));
		}
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

	public function toCodeArray ():Array<Int> {
		var res = [];
		for (i in 0...length) {
			res.push(charCodeAt(i));
		}
		return res;
	}

	
	public static function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}

	
	public static function fromByteAccess (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}

	
	public static function fromCharCode( code : Int ) : Utf8
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
		return fromByteAccess(bytes);
	}

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}

	
	public static function fromNativeString (s:String):Utf8 {

 		#if python
 		return fromByteAccess(ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8")));
 		#elseif (js || flash)
 		return EncodingTools.ucs2ToUtf8( new Ucs2(s));
 		#else
 		return fromByteAccess(ByteAccess.fromBytes(Bytes.ofString(s)));
 		#end
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

	function compare (other:Utf8):Int {
		var len1 = length;
		var len2 = other.length;
		var min = Std.int(Math.min(len1, len2));
		for (i in 0...min) {
			var a = charCodeAt(i);
			var b = other.charCodeAt(i);
			if (a < b) return -1;
			if (a > b) return 1;
		}
		if (len1 < len2) return -1;
		if (len1 > len2) return 1;
		return 0;
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
}

//#end
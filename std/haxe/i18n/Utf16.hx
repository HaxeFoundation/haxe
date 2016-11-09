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
	Cross platform Utf16 string API.
**/

#if (java || cs)
import haxe.io.Bytes;
import haxe.io.BytesData;
abstract Utf16(String) {

	/**/
	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = str;
	}

		function get_length() {
		return this.length;
	}

	public function toUpperCase() : Utf16 {
		return new Utf16(this.toUpperCase());
	}

	public function toLowerCase() : Utf16 {
		return new Utf16(this.toLowerCase());
	}

	public function isValidUcs2 ():Bool {
		for (i in 0...this.length) {
			var code = charCodeAt(i);
			if (EncodingTools.getUtf16CodeSize(code) == 4) return false;
		}
		return true;
	}

	public function charAt(index : Int) : Utf16 {
		return new Utf16(this.charAt(index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	public function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return this.lastIndexOf(str.toNativeString(),startIndex);
	}

	public function split( delimiter : Utf16 ) : Array<Utf16> {
		return cast this.split(delimiter.toNativeString());
	}

	public function substr( pos : Int, ?len : Int ) : Utf16 {
		return new Utf16(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return new Utf16(this.substring(startIndex,endIndex));
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return this == other.toNativeString();
	}

	@:op(A + B) inline function opAdd (other:Utf16) {
		return fromNativeString(this + other.toNativeString());
	}

	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	public static function fromCharCode( code : Int ) : Utf16 {

		return fromByteAccess(EncodingTools.charCodeToUtf16ByteAccess(code));
	}

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromNativeString (s:String):Utf16 {
		return new Utf16(s);
	}

	public function toNativeString() : String {
		return this;
	}

	public static inline function asByteAccess (s:Utf16):ByteAccess {
		return ByteAccess.fromBytes(s.toBytes());
	}

	public static inline function fromByteAccess (bytes:ByteAccess):Utf16 {
		#if cs
		return new Utf16(cs.system.text.Encoding.BigEndianUnicode.GetString(bytes.asBytesData(), 0, bytes.asBytesData().length));
		#elseif java
		try
			return new Utf16(new String(bytes.asBytesData(), 0, bytes.asBytesData().length, "UTF-16BE"))
		catch (e:Dynamic) throw e;
		#end

	}

	public function toUcs2() : Ucs2 {
		return EncodingTools.utf16ToUcs2(new Utf16(this));
	}

	public function toCodeArray () {
		return ByteAccess.fromBytes(toBytes()).toString();
	}


	 	public function toUtf8 ():Utf8 {
		return EncodingTools.utf16ToUtf8(new Utf16(this));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		#if cs
		var b = cs.system.text.Encoding.BigEndianUnicode.GetBytes(this);
		return Bytes.ofData(b);
		#elseif java
		try
		{
			var b:haxe.io.BytesData = untyped this.getBytes("UTF-16BE");
			return Bytes.ofData(b);
		}
		catch (e:Dynamic) throw e;
		#end
	}

	@:op(A > B) inline function opGreaterThan (other:Utf16) {
		return this > other.toNativeString();
	}
	@:op(A < B) inline function opLessThan (other:Utf16) {
		return this < other.toNativeString();
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf16) {
		return this <= other.toNativeString();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf16) {
		return this >= other.toNativeString();
	}
}

#else
// bytes based implementation

@:allow(haxe.i18n)
abstract Utf16(ByteAccess) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = asByteAccess(Utf16.fromNativeString(str));
	}

	public function toUpperCase() : Utf16 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = getNext2Bytes(i);
			var size = getCharSize(b);
			toUpperCaseLetter(this, res, i, size);
			i += size;
		}
		return fromByteAccess(res);
	}
	public function toLowerCase() : Utf16 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = getNext2Bytes(i);
			var size = getCharSize(b);
			toLowerCaseLetter(this, res, i, size);

			i += size;
		}
		return fromByteAccess(res);
	}

	inline function getNext2Bytes (pos:Int) {
		var b1 = this.fastGet(pos);
		var b2 = this.fastGet(pos+1);
		return (b1 << 8) | b2;
	}

	public function charAt(index : Int) : Utf16 {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < this.length) {
			var b = getNext2Bytes(i);
			var size = getCharSize(b);
			if (pos == index) {
				res = Utf16.fromByteAccess(this.sub(i, size));
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty() : res;
	}

	public function toCodeArray () {
		var res = [];
		for (i in 0...length) {
			res.push(charCodeAt(i));
		}
		return res;
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
	public function indexOf( str : Utf16, ?startIndex : Int ) : Int
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

	public inline function reverse ():Utf16 {
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
	public function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		var rev = fromByteAccess(this).reverse();
		var strRev = str.reverse();
		var thisLength = fromByteAccess(this).length;
		var sIndex = startIndex != null ? (thisLength - 1) - startIndex - (strRev.length - 1) : null;
		var i = rev.indexOf(strRev, sIndex);
		var res = i > -1 ? ((thisLength - 1) - (i + strRev.length - 1)) : i;
		return res;

	}
	public function split( delimiter : Utf16 ) : Array<Utf16>
	{
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();

		var res:Array<Utf16> = [];
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
	public function substr( pos : Int, ?len : Int ) : Utf16 {

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

		//trace("pos: " + pos, "len: " + len);



		var buf = new ByteAccessBuffer();

		var str = this;

		var cur = 0;

		var byteLength = str.length;
		var i = 0;

		while (i < byteLength) {
			var char = getNext2Bytes(i);
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
		//trace(buf);
		return fromByteAccess(buf.getByteAccess());
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
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

		if (startIndex == null || startIndex > len) return new Utf16("");
		
		return substr(startIndex, endIndex - startIndex);
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return this.equal(asByteAccess(other));
	}

	@:op(A + B) inline function opAdd (other:Utf16) {
		return fromByteAccess(this.append(asByteAccess(other)));
	}

	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	// private helpers

	function get_length() {
		var len = 0;
		var index = 0;
		while (index < this.length) {
			var size = getCharSize(getNext2Bytes(index));

			len++;
			index += size;
		}
		return len;
	}

	static inline function getCharSize (start2Bytes:Int):Int {
		return if (EncodingTools.isHighSurrogate(start2Bytes)) 4
		else 2;
	}

	static inline function isUpperCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b1 = bytes.fastGet(pos);
		var b2 = bytes.fastGet(pos+1);
		return b1 == 0x00 && b2 >= 0x41 && b2 <= 0x5A;
	}

	static inline function isLowerCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b1 = bytes.fastGet(pos);
		var b2 = bytes.fastGet(pos+1);
		return b1 == 0x00 && b2 >= 0x61 && b2 <= 0x7A;
	}

	static inline function toLowerCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos));
			target.set(pos+1, bytes.fastGet(pos+1)+0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	static inline function toUpperCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos));
			target.set(pos+1, bytes.fastGet(pos+1)-0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	static inline function empty () {
		return new Utf16("");
	}

	static inline function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {

			case 2: b.getInt16(pos);
			case 4: b.getInt32(pos);
			case _: throw "invalid byte sequence";
		}
	}

	static inline function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	static inline function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(bytes.fastGet(pos+i));
		}
	}

	public static inline function asByteAccess (s:Utf16):ByteAccess {
		return cast s;
	}

	public static inline function fromByteAccess (bytes:ByteAccess):Utf16 {
		return cast bytes;
	}

	public static function fromCharCode( code : Int ) : Utf16
	{
		return fromByteAccess(EncodingTools.charCodeToUtf16ByteAccess(code));
	}

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}


	public static function fromNativeString (s:String):Utf16 {
 		#if python
 		return fromByteAccess(ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-16be")));
		#elseif (neko || cpp || php)
		return EncodingTools.utf8ToUtf16(new Utf8(s));
 		#elseif (js || flash)
 		return EncodingTools.ucs2ToUtf16( new Ucs2(s));
 		#else
 		return fromByteAccess(ByteAccess.fromBytes(haxe.io.Bytes.ofString(s)));
 		#end
 	}

 	public function toNativeString() : String {
		return this.getString(0, this.length);
	}

 	public function toUcs2() : Ucs2 {
		return EncodingTools.utf16ToUcs2(fromByteAccess(this));
	}

	public function toUtf8 ():Utf8 {
		return EncodingTools.utf16ToUtf8(fromByteAccess(this));
	}

	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	function compare (other:Utf16):Int {
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
}

#end
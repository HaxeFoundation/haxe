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

import haxe.io.Bytes;

/**
	Cross platform Utf8 string API.
**/
@:allow(haxe.i18n)
abstract Utf8(ByteAccess) {

	@:extern public var length(get,never) : Int;

	/*@:extern inline*/
	public function new(str:String) : Void {
		this = asByteAccess(Utf8.fromNativeString(str));
	}

	/*@:extern inline*/
	public function toUpperCase() : Utf8 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			toUpperCaseLetter(this, res, i, size);

			i += size;
		}
		return wrapAsUtf8(res);
	}
	/*@:extern inline*/
	public function toLowerCase() : Utf8 {
		var res = ByteAccess.alloc(this.length);
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			toLowerCaseLetter(this, res, i, size);

			i += size;
		}
		return wrapAsUtf8(res);
	}
	/*@:extern inline*/
	public function charAt(index : Int) : Utf8 {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < this.length) {
			var b = this.fastGet(i);
			var size = getCharSize(b);
			if (pos == index) {
				res = Utf8.wrapAsUtf8(this.sub(i, size));
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty() : res;
	}
	/*@:extern inline*/
	public function charCodeAt( index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r = null;
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
	/*@:extern inline*/
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
		return wrapAsUtf8(res);
	}
	/*@:extern inline*/
	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		var rev = wrapAsUtf8(this).reverse();
		var strRev = str.reverse();
		var thisLength = wrapAsUtf8(this).length;
		var sIndex = startIndex != null ? (thisLength - 1) - startIndex - (strRev.length - 1) : null;
		var i = rev.indexOf(strRev, sIndex);
		var res = i > -1 ? ((thisLength - 1) - (i + strRev.length - 1)) : i;
		return res;

	}
	/*@:extern inline*/
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
					res.push(wrapAsUtf8(buf.getByteAccess()));
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
			res.push(wrapAsUtf8(buf.getByteAccess()));
		} else {
			res.push(empty());
		}
		return res;
	}
	/*@:extern inline*/
	public function substr( pos : Int, ?len : Int ) : Utf8 {

		if (pos < 0) {
			var thisLength = wrapAsUtf8(this).length;
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (len != null && len <= 0) {

			return new Utf8("");
		}

		//trace("pos: " + pos, "len: " + len);



		var buf = new ByteAccessBuffer();

		var str = this;

		var cur = 0;

		var byteLength = str.length;
		var i = 0;

		while (i < byteLength) {
			var char = str.fastGet(i);
			if (char == null) throw "error";
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
		return wrapAsUtf8(buf.getByteAccess());
	}

	/*@:extern inline*/
	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		return if (endIndex == null) {
			substr(startIndex, null);
		} else if (endIndex > startIndex) {
			substr(startIndex, endIndex - startIndex);
		} else empty();
	}
	/*@:extern inline*/
	public function toNativeString() : String {
		return this.getString(0, this.length);
	}

	@:extern public static inline function fromCharCode( code : Int ) : Utf8
	{
		var size = getCodeSize(code);
		var bytes = ByteAccess.alloc(size);
		switch size {
			case 1:
				bytes.set(0, code);
			case 2:
				bytes.set(0, code & 0xFF00);
				bytes.set(1, code & 0x00FF);
			case 3:
				bytes.set(0, (code & 0xFF0000) >> 16);
				bytes.set(1, (code & 0x00FF00) >> 8);
				bytes.set(2, code & 0x0000FF);
			case _: throw "invalid char code";
		}
		return wrapAsUtf8(bytes);
	}
	/*@:extern inline*/
	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	@:extern public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return wrapAsUtf8(ByteAccess.fromBytes(bytes).copy());
	}
	/*@:extern inline*/
	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(wrapAsUtf8(this));
	}

	@:op(A == B) inline function opEq (other:Utf8) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}

	// private helpers

	/*@:extern inline*/ function get_length() {
		var len = 0;
		var index = 0;
		while (index < this.length) {
			var size = getCharSize(this.fastGet(index));
			len++;
			index += size;
		}
		return len;
	}

	@:extern static inline function getCharSize (start:Int):Int {
		return if (start < 0x80) 1
		else if ((start & 0xE0) == 0xE0) 3
		else if ((start & 0xC0) == 0xC0) 2
		else throw "invalid utf8";
	}

	@:extern static inline function isUpperCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = bytes.fastGet(pos);
		return b >= 0x41 && b <= 0x5A;
	}

	@:extern static inline function isLowerCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = bytes.fastGet(pos);
		return b >= 0x61 && b <= 0x7A;
	}

	@:extern static inline function toLowerCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos)+0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	@:extern static inline function toUpperCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos)-0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	@:extern static inline function empty () {
		return new Utf8("");
	}

	@:extern static inline function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {
			case 1: b.fastGet(pos);
			case 2: (b.fastGet(pos) << 8) | (b.fastGet(pos+1));
			case 3: (b.fastGet(pos) << 16) | (b.fastGet(pos+1) << 8) | b.fastGet(pos+2);
			case _: throw "invalid byte sequence";
		}
	}

	@:extern static inline function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	@:extern static inline function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(bytes.fastGet(pos+i));
		}
	}

	@:extern static inline function getCodeSize (code:Int):Int {
		return
			if (code > 0xFFFF && code <= 0xFFFFFF) 3
			else if (code > 0xFF && code <= 0xFFFF) 2
			else if (code <= 0xFF) 1
			else throw "invalid code";
	}

	@:extern static inline function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}

	@:extern static inline function wrapAsUtf8 (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}

	@:extern public static inline function fromNativeString (s:String):Utf8 {

 		#if python
 		return wrapAsUtf8(ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8")));
 		#elseif js
 		return EncodingTools.ucs2ToUtf8( new Ucs2(s));
 		#else

 		return wrapAsUtf8(ByteAccess.fromBytes(Bytes.ofString(s)));
 		#end
 	}
}
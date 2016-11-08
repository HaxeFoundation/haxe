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

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = str;
	}

	function get_length():Int {
		return this.length;
	}

	public function toUpperCase() : Ucs2 {
		return new Ucs2(this.toUpperCase());
	}

	public function toLowerCase() : Ucs2 {
		return new Ucs2(this.toLowerCase());
	}

	public function charAt(index : Int) : Ucs2 {
		return new Ucs2(this.charAt(index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	public function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.toNativeString());
		}
		return this.lastIndexOf(str.toNativeString(), startIndex);
		
	}

	public function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return cast this.split(delimiter.toNativeString());
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		if (len == null) return new Ucs2(this.substr(pos)); // required for flash
		return new Ucs2(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		if (endIndex == null) return new Ucs2(this.substring(startIndex));
		return new Ucs2(this.substring(startIndex,endIndex));
	}

	public function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return new Ucs2(String.fromCharCode(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		var b = haxe.io.Bytes.alloc(length*2);
		for (i in 0...length) {
			var code = charCodeAt(i);
			if (code == null) throw "assert";
			b.set(i * 2, ((code & 0xFF00) >> 8));
			b.set(i * 2 + 1, (code & 0x00FF));
		}
		return b;
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {

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


	@:op(A == B) inline function opEq (other:Ucs2) {
		return this == other.toNativeString();
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return fromNativeString(this + other.toNativeString());
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	public function toUtf8() : Utf8 {
		return EncodingTools.ucs2ToUtf8(new Ucs2(this));
	}

	public static function wrapAsUcs2(s:String) return new Ucs2(s);

	public function toUtf16() : Utf16 {
		return EncodingTools.ucs2ToUtf16(wrapAsUcs2(this));
	}

	public static inline function fromNativeString (str:String):Ucs2 {
		return new Ucs2(str);
	}

	public function toByteString ():String {
		var res = [];
		for (i in 0...length) {
			res.push(charCodeAt(i));
		}
		return res.join(",");
	}


}
#else

import haxe.i18n.EncodingTools;
import haxe.i18n.ByteAccess;

@:allow(haxe.i18n)
abstract Ucs2(ByteAccess) {

	public var length(get,never) : Int;

	public static inline function fromNativeString (str:String):Ucs2 {
		#if (js || flash)
			throw "assert"
		#elseif (neko || cpp || python || php)
			var bytes = ByteAccess.fromBytes(haxe.io.Bytes.ofString(str));
			//trace(bytes);
			var ucs2Bytes = EncodingTools.utf8ByteAccessToUcs2ByteAccess(bytes);
			//trace(ucs2Bytes);
			return Ucs2.wrapAsUcs2(ucs2Bytes);
		#elseif (java || cs)
			return new Utf16(str).toUcs2();
		#elseif cs
			return throw "not implemented";
		#end
	}

	public static function asByteAccess( s:Ucs2 ) : ByteAccess {
		return cast s;
	}

	public function toByteString () {
		return ByteAccess.fromBytes(toBytes()).toString();
	}

	


	public function new(str:String)  {
		this = haxe.i18n.Ucs2.asByteAccess(haxe.i18n.Ucs2.fromNativeString(str));
	}

function get_length():Int {
		return this.length >> 1;
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

	public function toUpperCase() : Ucs2 {
		var buffer = new ByteAccessBuffer();
		var i = 0;
		while (i < this.length) {
			var byte1 = this.fastGet(i);
			var byte2 = this.fastGet(i+1);
			//trace(byte1);
			//trace(byte2);
			i+=2;
			var newBytes = toUpperCaseLetter( (byte1 << 8) | byte2);
			//trace(newBytes);
			var newByte1 = (newBytes & 0xFF00) >> 8;
			var newByte2 = newBytes & 0x00FF;
			//trace(newByte1);
			//trace(newByte2);
			buffer.addByte(newByte1);
			buffer.addByte(newByte2);
		}

		return Ucs2.wrapAsUcs2(buffer.getByteAccess());
	}

	public function toLowerCase() : Ucs2 {
		var buffer = new ByteAccessBuffer();
		var i = 0;
		while (i < this.length) {
			var byte1 = this.fastGet(i);
			var byte2 = this.fastGet(i+1);
			i+=2;
			var newBytes = toLowerCaseLetter( (byte1 << 8) | byte2);
			buffer.addByte((newBytes & 0xFF00) >> 8);
			buffer.addByte(newBytes & 0x00FF);
		}

		return Ucs2.wrapAsUcs2(buffer.getByteAccess());
	}

	public function charAt(index : Int) : Ucs2 {
		if (index < 0 || index >= wrapAsUcs2(this).length) {
			return new Ucs2("");
		}
		var b = ByteAccess.alloc(2);
		b.set(0, this.get(index * 2));
		b.set(1, this.get(index * 2 + 1));

		return Ucs2.wrapAsUcs2(b);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		if (index < 0 || index >= wrapAsUcs2(this).length) {
			return null;
		}
		return (this.get(index << 1) << 8) | this.get((index << 1) + 1);
	}

	public function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		var res = -1;
		var str = asByteAccess(str);
		var strLen = str.length;

		var len = this.length;
		var sIndex = startIndex != null ? startIndex * 2 : 0;
		var pos = 0;
		var fullPos = sIndex;
		var i = sIndex;
		while (i < len) {

			if (this.fastGet(i) == str.fastGet(pos)) {
				pos++;
			} else {
				pos = 0;
			}
			fullPos++;
			if (pos == strLen) {
				res = (fullPos - strLen) >> 1;
				break;
			}
			i++;
		}
		return res;
	}

	public function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		var str = asByteAccess(str);
		var len = str.length;
		var pos = len-1;
		//trace(startIndex);
		//trace("str: " + str, "str.length: " + len);

		var startIndex = startIndex == null ? this.length : ((startIndex) << 1)+len;

		//trace(startIndex >> 1);
		if (startIndex > this.length) {
			startIndex = this.length;
		}
		var i = startIndex;
		var res = -1;
		var fullPos = startIndex;
		while (--i > -1) {
			//trace(i);
			//trace(pos);
			if (this.fastGet(i) == str.fastGet(pos)) {
				pos--;
			} else {
				pos = len-1;
			}
			fullPos--;
			if (pos == -1) {
				res = (fullPos) >> 1;
				break;
			}
		}
		return res;
	}

	public function split( delimiter : Ucs2 ) : Array<Ucs2> {
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

		if (pos != 0) {
			buffer.addBuffer(tempBuffer);
		}
		if (buffer.length > 0) {
			res.push(Ucs2.wrapAsUcs2(buffer.getByteAccess()));
		} else {
			res.push(new Ucs2(""));
		}
		return res;
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		return if (len == null) {
			if (pos < 0) {
				var newPos = wrapAsUcs2(this).length + pos;
				if (newPos < 0) newPos = 0;
				substring(newPos);
			} else {
				substring(pos);
			}


		} else {
			if (len < 0) {
				substring(pos, wrapAsUcs2(this).length + len);
			} else {
				substring(pos, pos + len);
			}
		}
	}


	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		var b = this;



		if (startIndex < 0) startIndex = 0;


		if (endIndex == null) {
			endIndex = wrapAsUcs2(this).length;
		}
		else if (endIndex < 0) {
			endIndex = 0;
		} else if (endIndex > wrapAsUcs2(this).length) {
			endIndex = wrapAsUcs2(this).length;
		}


		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		return wrapAsUcs2(b.sub(startIndex * 2, endIndex * 2 - startIndex * 2));
	}
	// private helpers
	static inline function wrapAsUcs2 (bytes:ByteAccess):Ucs2 {
		return cast bytes;
	}
	public function toNativeString() : String {
		// Ucs2 to Utf8
		return EncodingTools.ucs2ToUtf8(wrapAsUcs2(this)).toNativeString();
	}

	static inline function getCodeSize (code:Int):Int {
		return if (code <= 0xFFFF) 2 else 4;
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		var size = getCodeSize(code);
		var bytes = ByteAccess.alloc(size);
		switch size {
			case 2:
				bytes.set(0, code & 0xFF00);
				bytes.set(1, code & 0x00FF);
			case 4:
				throw "no surrogate pairs allowed";

			case _: throw "invalid char code";
		}
		return wrapAsUcs2(bytes);
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return wrapAsUcs2(ByteAccess.fromBytes(bytes).copy());
	}

	public function toUtf8() : Utf8 {
		return EncodingTools.ucs2ToUtf8(wrapAsUcs2(this));
	}

	public function toUtf16() : Utf16 {
		return EncodingTools.ucs2ToUtf16(wrapAsUcs2(this));
	}

	
	@:op(A == B) inline function opEq (other:Ucs2) {
		return this.equal(asByteAccess(other));
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return wrapAsUcs2(this.append(asByteAccess(other)));
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}
	

	
}

#end
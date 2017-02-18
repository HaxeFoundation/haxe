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

import haxe.i18n.ByteAccess;
import haxe.i18n.Ucs2.Ucs2Impl;

@:allow(haxe.i18n)
class Ucs2Tools {

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

	static function toUpperCase(impl:Ucs2Impl) : Ucs2Impl {
		var res = Ucs2Impl.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl.getInt16(i);
			res.setInt16(i, toUpperCaseLetter(b));
			i+=2;
			
		}
		return res;
	}

	static function toLowerCase(impl:Ucs2Impl) : Ucs2Impl {
		var res = Ucs2Impl.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = impl.getInt16(i);
			res.setInt16(i, toLowerCaseLetter(b));
			i+=2;
		}

		return res;
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
		
		return impl.sub(startIndex * 2, endIndex * 2 - startIndex * 2);
	}

	public static function split( impl:Ucs2Impl, delimiter : Ucs2Impl ) : Array<Ucs2> {
		var delimiterLen = delimiter.length;

		var buffer = new ByteAccessBuffer();
		var tempBuffer = new ByteAccessBuffer();

		var res = [];
		var pos = 0;

		for ( i in 0...impl.length) {

			var b = impl.fastGet(i);
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
				if (buffer.length > 0) {
					res.push(Ucs2.fromImpl(buffer.getByteAccess()));
					buffer.reset();
				} else {
					res.push(Ucs2.fromImpl(empty));
				}
				tempBuffer.reset();
			}
		}

		if (pos != 0) {
			buffer.addBuffer(tempBuffer);
		}
		if (buffer.length > 0) {
			res.push(Ucs2.fromImpl(buffer.getByteAccess()));
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
	/*
	static function compare (impl:Ucs2Impl, other:Ucs2Impl):Int {
		var len1 = strLength(impl);
		var len2 = strLength(other);
		
		var min = len1 < len2 ? len1 : len2;
		for (i in 0...min) {
			var a = fastCodeAt(impl, i);
			var b = fastCodeAt(other, i);
			if (a < b) return -1;
			if (a > b) return 1;
		}
		if (len1 < len2) return -1;
		if (len1 > len2) return 1;
		return 0;
	}
	*/
	
	static function isValid(impl:Ucs2Impl) {
		for (i in 0...strLength(impl)) {
			var code = fastCodeAt(impl, i);
			if (Encoding.isHighSurrogate(code)) return false;
		}
		return true;
	}
	
}


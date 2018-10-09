/*
 * Copyright (C)2005-2018 Haxe Foundation
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
package haxe.iterators;

import php.Global.*;
import php.NativeString;

class StringKeyValueIteratorUnicode {
	var charOffset:Int = 0;
	var byteOffset:Int = 0;
	var totalBytes:Int;
	var s:NativeString;

	public inline function new(s:String) {
		this.s = s;
		totalBytes = strlen(s);
	}

	public inline function hasNext() {
		return byteOffset < totalBytes;
	}

	public inline function next() {
		var code = ord(s[byteOffset]);
		if(code < 0xC0) {
			byteOffset++;
		} else if(code < 0xE0) {
			code = ((code - 0xC0) << 6) + ord(s[byteOffset + 1]) - 0x80;
			byteOffset += 2;
		} else if(code < 0xF0) {
			code = ((code - 0xE0) << 12) + ((ord(s[byteOffset + 1]) - 0x80) << 6) + ord(s[byteOffset + 2]) - 0x80;
			byteOffset += 3;
		} else {
			code = ((code - 0xF0) << 18) + ((ord(s[byteOffset + 1]) - 0x80) << 12) + ((ord(s[byteOffset + 2]) - 0x80) << 6) + ord(s[byteOffset + 3]) - 0x80;
			byteOffset += 4;
		}
		return { key: charOffset++, value: code };
	}

	static public inline function unicodeKeyValueIterator(s:String) {
		return new StringKeyValueIteratorUnicode(s);
	}
}
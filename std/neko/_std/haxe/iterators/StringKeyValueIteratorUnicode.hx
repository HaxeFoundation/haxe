/*
 * Copyright (C)2005-2019 Haxe Foundation
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

class StringKeyValueIteratorUnicode {
	var byteOffset:Int = 0;
	var charOffset:Int = 0;
	var s:String;

	public inline function new(s:String) {
		this.s = s;
	}

	public inline function hasNext() {
		return byteOffset < s.length;
	}

	public inline function next() {
		var code:Int = codeAt(byteOffset);
		if (code < 0xC0) {
			byteOffset++;
		} else if (code < 0xE0) {
			code = ((code - 0xC0) << 6) + codeAt(byteOffset + 1) - 0x80;
			byteOffset += 2;
		} else if (code < 0xF0) {
			code = ((code - 0xE0) << 12) + ((codeAt(byteOffset + 1) - 0x80) << 6) + codeAt(byteOffset + 2) - 0x80;
			byteOffset += 3;
		} else {
			code = ((code - 0xF0) << 18)
				+ ((codeAt(byteOffset + 1) - 0x80) << 12)
				+ ((codeAt(byteOffset + 2) - 0x80) << 6)
				+ codeAt(byteOffset + 3)
				- 0x80;
			byteOffset += 4;
		}
		return {key: charOffset++, value: code};
	}

	inline function codeAt(index:Int):Int {
		return untyped $sget(s.__s, index);
	}

	static public inline function unicodeKeyValueIterator(s:String) {
		return new StringKeyValueIteratorUnicode(s);
	}
}

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

/**
	This iterator can be used to iterate across strings in a cross-platform
	way. It handles surrogate pairs on platforms that require it. On each
	iteration, it returns the next character offset as key and the next
	character code as value.

	Note that in the general case, because of surrogate pairs, the key values
	should not be used as offsets for various String API operations. For the
	same reason, the last key value returned might be less than `s.length - 1`.
**/
class StringKeyValueIteratorUnicode {
	var byteOffset = 0;
	var charOffset = 0;
	var s:String;

	/**
		Create a new `StringKeyValueIteratorUnicode` over String `s`.
	**/
	public inline function new(s:String) {
		this.s = s;
	}

	/**
		See `Iterator.hasNext`
	**/
	public inline function hasNext() {
		return byteOffset < s.length;
	}

	/**
		See `Iterator.next`
	**/
	@:access(StringTools)
	public inline function next() {
		#if utf16
		var c = StringTools.utf16CodePointAt(s, byteOffset++);
		if (c >= StringTools.MIN_SURROGATE_CODE_POINT) {
			byteOffset++;
		}
		return {key: charOffset++, value: c};
		#else
		return {key: charOffset++, value: StringTools.fastCodeAt(s, byteOffset++)};
		#end
	}

	/**
		Convenience function which can be used as a static extension.
	**/
	static public inline function unicodeKeyValueIterator(s:String) {
		return new StringKeyValueIteratorUnicode(s);
	}
}

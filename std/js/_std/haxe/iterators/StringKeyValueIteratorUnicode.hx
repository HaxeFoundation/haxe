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
	var nativeIter: js.lib.Iterator<String>;
	var current: {value: String, done: Bool};
	var charOffset = 0;

	/**
		Create a new `StringKeyValueIteratorUnicode` over String `s`.
	**/
	public inline function new(s:String) {
		this.nativeIter = js.lib.Symbol.iterator.ofObject(s)();
		this.current = this.nativeIter.next();
	}

	/**
		See `Iterator.hasNext`
	**/
	public inline function hasNext() {
		return !this.current.done;
	}

	/**
		See `Iterator.next`
	**/
	@:access(StringTools)
	public inline function next() {
		var c = StringTools.utf16CodePointAt(this.current.value, 0);
		this.current = nativeIter.next();
		return {key: charOffset++, value: c};
	}

	/**
		Convenience function which can be used as a static extension.
	**/
	static public inline function unicodeKeyValueIterator(s:String) {
		return new StringKeyValueIteratorUnicode(s);
	}
}

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

package js.lib;

/**
	`HaxeIterator` wraps a JavaScript native iterator object to enable for-in iteration in haxe.
	It can be used directly: `new HaxeIterator(jsIterator)` or via using: `using HaxeIterator`.
**/
class HaxeIterator<T> {

	final jsIterator: js.lib.Iterator<T>;
	var lastStep: js.lib.Iterator.IteratorStep<T>;

	public inline function new(jsIterator: js.lib.Iterator<T>) {
		this.jsIterator = jsIterator;
		lastStep = jsIterator.next();
	}

	public inline function hasNext(): Bool {
		return !lastStep.done;
	}

	public inline function next(): T {
		var v = lastStep.value;
		lastStep = jsIterator.next();
		return v;
	}

	public static inline function iterator<T>(jsIterator: js.lib.Iterator<T>) {
		return new HaxeIterator(jsIterator);
	}

}
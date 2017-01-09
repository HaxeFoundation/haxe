/*
 * Copyright (C)2005-2017 Haxe Foundation
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

 /**
	IntIterator is used for implementing interval iterations.

	It is usually not used explicitly, but through its special syntax:
	`min...max`

	While it is possible to assign an instance of IntIterator to a variable or
	field, it is worth noting that IntIterator does not reset after being used
	in a for-loop. Subsequent uses of the same instance will then have no
	effect.

	@see https://haxe.org/manual/lf-iterators.html
**/
class IntIterator {

	var min : Int;
	var max : Int;

	/**
		Iterates from `min` (inclusive) to `max` (exclusive).

		If `max <= min`, the iterator will not act as a countdown.
	**/
	public inline function new( min : Int, max : Int ) {
		this.min = min;
		this.max = max;
	}

	/**
		Returns true if the iterator has other items, false otherwise.
	**/
	public inline function hasNext() {
		return min < max;
	}

	/**
		Moves to the next item of the iterator.

		If this is called while hasNext() is false, the result is unspecified.
	**/
	public inline function next() {
		return min++;
	}

}

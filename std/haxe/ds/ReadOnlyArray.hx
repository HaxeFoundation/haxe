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

package haxe.ds;

/**
	`ReadOnlyArray` is an abstract over an ordinary `Array` which only exposes
	APIs that don't modify the instance, hence "read-only".

	Note that this doesn't necessarily mean that the instance is *immutable*.
	Other code holding a reference to the underlying `Array` can still modify it,
	and the reference can be obtained with a `cast`.
**/
@:forward(copy, filter, indexOf, iterator, keyValueIterator, join, lastIndexOf, map, slice, contains, toString)
abstract ReadOnlyArray<T>(Array<T>) from Array<T> to Iterable<T> {
	/**
		The length of `this` Array.
	**/
	public var length(get, never):Int;

	inline function get_length()
		return this.length;

	@:arrayAccess inline function get(i:Int)
		return this[i];

	/**
		Returns a new Array by appending the elements of `a` to the elements of
		`this` Array.

		This operation does not modify `this` Array.

		If `a` is the empty Array `[]`, a copy of `this` Array is returned.

		The length of the returned Array is equal to the sum of `this.length`
		and `a.length`.

		If `a` is `null`, the result is unspecified.
	**/
	public inline function concat(a:ReadOnlyArray<T>):Array<T> {
		return this.concat(cast a);
	}
}

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
	The `WeakSet` object lets you store weakly held objects in a collection.

	Documentation [WeakSet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WeakSet")
extern class WeakSet {
	/**
		The value of the `length` property is 0.
	**/
	static final length:Int;

	/**
		If an iterable object is passed, all of its elements will be added to the new WeakSet.
		null is treated as undefined.
	**/
	@:pure function new(?iterable:Any);

	/**
		Appends a new object with the given value to the `WeakSet` object.
	**/
	function add(value:{}):WeakSet;

	/**
		Removes the element associated to the `value`.
		`has(value)` will return `false` afterwards.
	**/
	function delete(value:{}):Bool;

	/**
		Returns a boolean asserting whether an element is present with the given value
		in the `WeakSet` object or not.
	**/
	@:pure function has(value:{}):Bool;
}

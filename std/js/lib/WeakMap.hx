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
	The `WeakMap` object is a collection of key/value pairs in which the keys are weakly referenced.
	The keys must be objects and the values can be arbitrary values.

	You can learn more about WeakMaps in the section [WeakMap object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Keyed_collections#WeakMap_object)
	in [Keyed collections](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Keyed_collections).

	Documentation [WeakMap](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WeakMap")
extern class WeakMap<T> {
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
		Removes any value associated to the `key`. `has(key)` will return `false` afterwards.
	**/
	function delete(key:{}):Bool;

	/**
		Returns the value associated to the `key`, or `undefined` if there is none.
	**/
	@:pure function get(key:{}):T;

	/**
		Returns a Boolean asserting whether a value has been associated to the `key` in the `WeakMap` object or not.
	**/
	@:pure function has(key:{}):Bool;

	/**
		Sets the value for the `key` in the `WeakMap` object. Returns the `WeakMap` object.
	**/
	function set(key:{}, value:T):WeakMap<T>;
}

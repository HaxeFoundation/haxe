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
	The (native) JavaScript Map object holds key-value pairs.
	Any value (both objects and primitive values) may be used as either a key
	or a value.

	Documentation [Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Map")
extern class Map<K, V> {
	/**
		The number of key/value pairs in the `js.Map` object.
	**/
	var size(default, null):Int;

	/**
		An Array or other iterable object whose elements are key-value pairs
		(arrays with two elements, e.g. `[[ 1, 'one' ],[ 2, 'two' ]]`).
		Each key-value pair is added to the new `js.Map`;
		null values are treated as undefined.
	**/
	@:pure function new(?iterable:Any);

	/**
		A boolean asserting whether a value has been associated to the key in
		the `js.Map` object or not.
	**/
	@:pure function has(key:K):Bool;

	/**
		The value associated to the key, or `null` if there is none.
	**/
	@:pure function get(key:K):Null<V>;

	/**
		Sets the value for the key in the Map object.
		Returns the `js.Map` object.
	**/
	function set(key:K, value:V):Map<K, V>;

	/**
		Returns `true` if an element in the `js.Map` object existed and has been
		removed, or `false` if the element does not exist.
		`has(key)` will return `false` afterwards.
	**/
	function delete(key:K):Bool;

	/**
		Removes all key/value pairs from the Map object.
	**/
	function clear():Void;

	/**
		Calls `callback` once for each key-value pair present in the `js.Map`
		object, in insertion order.

		If a `thisArg` parameter is provided to forEach, it will be used as the
		`this` value for each callback.
	**/
	function forEach(callback:(value:V, key:K, map:Map<K, V>) -> Void, ?thisArg:Any):Void;

	/**
		Returns a new `Iterator` object that contains the keys for each element
		in the `js.Map` object in insertion order.
	**/
	function keys():js.lib.Iterator<K>;

	/**
		Returns a new `Iterator` object that contains the values for each
		element in the `js.Map` object in insertion order.
	**/
	function values():js.lib.Iterator<V>;

	/**
		Returns a new `Iterator` object that contains an array of `KeyValue`
		for each element in the `js.Map` object in insertion order.
	**/
	function entries():js.lib.Iterator<KeyValue<K, V>>;

	inline function iterator():js.lib.HaxeIterator<V> {
		return new HaxeIterator(this.values());
	}

	inline function keyValueIterator():HaxeIterator<KeyValue<K, V>> {
		return new HaxeIterator(this.entries());
	}
}

@:deprecated typedef MapEntry<K, V> = KeyValue<K, V>;
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
	The `js.Set` object lets you store unique values of any type, whether
	primitive values or object references.

	Documentation [Set](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Set")
extern class Set<T> {
	/**
		The number of values in the `js.Set` object.
	**/
	var size(default, null):Int;

	/**
		If an iterable object is passed, all of its elements will be added to
		the new `js.Set`.
	**/
	@:pure function new(?iterable:Any);

	/**
		Returns a boolean asserting whether an element is present with the given
		value in the `js.Set` object or not.
	**/
	@:pure function has(value:T):Bool;

	/**
		Appends a new element with the given value to the `js.Set` object.
		Returns the `js.Set` object.
	**/
	function add(value:T):Set<T>;

	/**
		Removes the element associated to the value and returns the value that
		`has(value)` would have previously returned.
		`has(value)` will return `false` afterwards.
	**/
	function delete(value:T):Bool;

	/**
		Removes all elements from the `js.Set` object.
	**/
	function clear():Void;

	/**
		Calls `callback` once for each key-value pair present in the `js.Set`
		object, in insertion order.

		If a `thisArg` parameter is provided to forEach, it will be used as the
		`this` value for each callback.
	**/
	function forEach(callback:(value:T, key:T, set:Set<T>) -> Void, ?thisArg:Any):Void;

	/**
		Returns a new `js.lib.Iterator` object that contains the keys for each element
		in the `js.Set` object in insertion order.
	**/
	function keys():js.lib.Iterator<T>;

	/**
		Returns a new `js.lib.Iterator` object that contains the values for each
		element in the `js.Set` object in insertion order.
	**/
	function values():js.lib.Iterator<T>;

	/**
		Returns a new `js.lib.Iterator` object that contains an array of
		`[value, value]` for each element in the `js.Set` object, in insertion
		order.
		This is kept similar to the `js.Map` object, so that each entry has the
		same value for its key and value here.
	**/
	function entries():js.lib.Iterator<KeyValue<T, T>>;

	inline function iterator():HaxeIterator<T> {
		return new HaxeIterator(this.values());
	}

	inline function keyValueIterator():SetKeyValueIterator<T> {
		return new SetKeyValueIterator(this);
	}
}

/**
	key => value iterator for js.lib.Set, tracking the entry index for the key to match the behavior of haxe.ds.List
**/
class SetKeyValueIterator<T> {
	final set:js.lib.Set<T>;
	final values:HaxeIterator<T>;
	var index = 0;

	public inline function new(set:js.lib.Set<T>) {
		this.set = set;
		this.values = new HaxeIterator(set.values());
	}

	public inline function hasNext():Bool {
		return values.hasNext();
	}

	public inline function next():{key:Int, value:T} {
		return {
			key: index++,
			value: values.next(),
		};
	}
}

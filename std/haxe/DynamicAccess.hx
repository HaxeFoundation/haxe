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
package haxe;

/**
	DynamicAccess is an abstract type for working with anonymous structures
	that are intended to hold collections of objects by the string key.

	For example, these types of structures are often created from JSON.

	Basically, it wraps `Reflect` calls in a `Map`-like interface.
**/
abstract DynamicAccess<T>(Dynamic<T>) from Dynamic<T> to Dynamic<T> {

	/**
		Creates a new structure.
	**/
	public inline function new() this = {};

	/**
		Returns a value by specified `key`.

		If the structure does not contain the given key, null is returned.

		If `key` is null, the result is unspecified.
	**/
	@:arrayAccess
	public inline function get(key:String):Null<T> {
		#if js
		return untyped this[key]; // we know it's an object, so we don't need a check
		#else
		return Reflect.field(this, key);
		#end
	}

	/**
		Sets a `value` for a specified `key`.

		If the structure contains the given key, its value will be overwritten.

		Returns the given value.

		If `key` is null, the result is unspecified.
	**/
	@:arrayAccess
	public inline function set(key:String, value:T):T {
		#if js
		return untyped this[key] = value;
		#else
		Reflect.setField(this, key, value);
		return value;
		#end
	}

	/**
		Tells if the structure contains a specified `key`.

		If `key` is null, the result is unspecified.
	**/
	public inline function exists(key:String):Bool return Reflect.hasField(this, key);

	/**
		Removes a specified `key` from the structure.

		Returns true, if `key` was present in structure, or false otherwise.

		If `key` is null, the result is unspecified.
	**/
	public inline function remove(key:String):Bool return Reflect.deleteField(this, key);

	/**
		Returns an array of `keys` in a structure.
	**/
	public inline function keys():Array<String> return Reflect.fields(this);
}

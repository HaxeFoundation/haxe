/*
 * Copyright (C)2005-2012 Haxe Foundation
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
	Hashtable over a set of elements, using [String] as keys.
	Other kind of keys are not possible on all platforms since they
	can't always be implemented efficiently.
**/
extern class StringMap<T> {

	/**
		Creates a new empty hashtable.
	**/
	public function new() : Void;

	/**
		Set a value for the given key.
	**/
	public function set( key : String, value : T ) : Void;

	/**
		Get a value for the given key.
	**/
	public function get( key : String ) : Null<T>;

	/**
		Tells if a value exists for the given key.
		In particular, it's useful to tells if a key has
		a [null] value versus no value.
	**/
	public function exists( key : String ) : Bool;

	/**
		Removes a hashtable entry. Returns [true] if
		there was such entry.
	**/
	public function remove( key : String ) : Bool;

	/**
		Returns an iterator of all keys in the hashtable.
	**/
	public function keys() : Iterator<String>;

	/**
		Returns an iterator of all values in the hashtable.
	**/
	public function iterator() : Iterator<T>;

	/**
		Returns an displayable representation of the hashtable content.
	**/
	public function toString() : String;

}

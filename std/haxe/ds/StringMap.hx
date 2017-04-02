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

package haxe.ds;

/**
	StringMap allows mapping of String keys to arbitrary values.

	See `Map` for documentation details.

	@see https://haxe.org/manual/std-Map.html
**/
extern class StringMap<T> implements haxe.Constraints.IMap<String,T> {

	/**
		Creates a new StringMap.
	**/
	public function new() : Void;

	/**
		See `Map.set`
	**/
	public function set( key : String, value : T ) : Void;

	/**
		See `Map.get`
	**/
	public function get( key : String ) : Null<T>;

	/**
		See `Map.exists`
	**/
	public function exists( key : String ) : Bool;

	/**
		See `Map.remove`
	**/
	public function remove( key : String ) : Bool;

	/**
		See `Map.keys`
	**/
	public function keys() : Iterator<String>;

	/**
		See `Map.iterator`
	**/
	public function iterator() : Iterator<T>;

	/**
		See `Map.toString`
	**/
	public function toString() : String;

}

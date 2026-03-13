/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.url;

import js.node.Iterator;

/**
	The `URLSearchParams` API provides read and write access to the query of a `URL`.
	The `URLSearchParams` class can also be used standalone with one of the four following constructors.
	The `URLSearchParams` class is also available on the global object.

	The WHATWG `URLSearchParams` interface and the `querystring` module have similar purpose,
	but the purpose of the querystring module is more general, as it allows the customization of delimiter characters (`&` and` `=`). On the other hand, this API is designed purely for URL query strings.
**/
@:jsRequire("url", "URLSearchParams")
extern class URLSearchParams {
	@:overload(function(init:String):Void {})
	@:overload(function(obj:Dynamic<String>):Void {})
	@:overload(function(array:Array<URLSearchParamsEntry>):Void {})
	@:overload(function(iter:Iterator<URLSearchParamsEntry>):Void {})
	function new():Void;

	/**
		Append a new name-value pair to the query string.
	**/
	function append(name:String, value:String):Void;

	/**
		Remove all name-value pairs whose name is `name`.
	**/
	function delete(name:String):Void;

	/**
		Returns an ES6 `Iterator` over each of the name-value pairs in the query.
		Each item of the iterator is a JavaScript `Array`.
		The first item of the `Array` is the `name`, the second item of the `Array` is the `value`.
	**/
	function entries():Iterator<URLSearchParamsEntry>;

	/**
		Iterates over each name-value pair in the query and invokes the given function.
	**/
	#if haxe4
	@:overload(function(fn:(value:String) -> Void, ?thisArg:Dynamic):Void {})
	@:overload(function(fn:(value:String, name:String) -> Void, ?thisArg:Dynamic):Void {})
	function forEach(fn:(value:String, name:String, searchParams:URLSearchParams) -> Void, ?thisArg:Dynamic):Void;
	#else
	@:overload(function(fn:String->Void, ?thisArg:Dynamic):Void {})
	@:overload(function(fn:String->String->Void, ?thisArg:Dynamic):Void {})
	function forEach(fn:String->String->URLSearchParams->Void, ?thisArg:Dynamic):Void;
	#end

	/**
		Returns the value of the first name-value pair whose name is `name`.
		If there are no such pairs, `null` is returned.
	**/
	function get(name:String):String;

	/**
		Returns the values of all name-value pairs whose name is `name`.
		If there are no such pairs, an empty array is returned.
	**/
	function getAll(name:String):Array<String>;

	/**
		Returns `true` if there is at least one name-value pair whose name is `name`.
	**/
	function has(name:String):Bool;

	/**
		Returns an ES6 `Iterator` over the names of each name-value pair.
	**/
	function keys():Iterator<String>;

	/**
		Sets the value in the `URLSearchParams` object associated with `name` to `value`.
		If there are any pre-existing name-value pairs whose names are `name`, set the first such pair's value to `value` and remove all others.
		If not, append the name-value pair to the query string.
	**/
	function set(name:String, value:String):Void;

	/**
		Sort all existing name-value pairs in-place by their names. Sorting is done with a [stable sorting algorithm](https://en.wikipedia.org/wiki/Sorting_algorithm#Stability),
		so relative order between name-value pairs with the same name is preserved.

		This method can be used, in particular, to increase cache hits.
	**/
	function sort():Void;

	/**
		Returns the search parameters serialized as a string, with characters percent-encoded where necessary.
	**/
	function toString():String;

	/**
		Returns an ES6 `Iterator` over the values of each name-value pair.
	**/
	function values():Iterator<String>;
}

/**
	The name-value pair access helper for `js.node.url.URLSearchParams.entries()`.
**/
abstract URLSearchParamsEntry(Array<String>) {
	public var name(get, never):String;
	public var value(get, never):String;

	public function new(name:String, value:String) {
		this = [name, value];
	}

	inline function get_name():String {
		return this[0];
	}

	inline function get_value():String {
		return this[1];
	}
}

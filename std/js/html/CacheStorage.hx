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

// This file is generated from mozilla\CacheStorage.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `CacheStorage` interface represents the storage for `Cache` objects.

	Documentation [CacheStorage](https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage>
**/
@:native("CacheStorage")
extern class CacheStorage {
	/** @throws DOMError */
	function new( namespace : CacheStorageNamespace, principal : Dynamic/*MISSING Principal*/ ) : Void;

	/**
		Checks if a given `Request` is a key in any of the `Cache` objects that the `CacheStorage` object tracks, and returns a `Promise` that resolves to that match.
	**/
	@:overload( function( request : String, ?options : CacheQueryOptions) : Promise<Response> {} )
	function match( request : Request, ?options : CacheQueryOptions ) : Promise<Response>;

	/**
		Returns a `Promise` that resolves to `true` if a `Cache` object matching the `cacheName` exists.
	**/
	function has( cacheName : String ) : Promise<Bool>;

	/**
		Returns a `Promise` that resolves to the `Cache` object matching the `cacheName` (a new cache is created if it doesn't already exist.)
	**/
	function open( cacheName : String ) : Promise<Cache>;

	/**
		Finds the `Cache` object matching the `cacheName`, and if found, deletes the `Cache` object and returns a `Promise` that resolves to `true`. If no `Cache` object is found, it returns `false`.
	**/
	function delete( cacheName : String ) : Promise<Bool>;

	/**
		Returns a `Promise` that will resolve with an array containing strings corresponding to all of the named `Cache` objects tracked by the `CacheStorage`. Use this method to iterate over a list of all the `Cache` objects.
	**/
	function keys() : Promise<Array<String>>;
}
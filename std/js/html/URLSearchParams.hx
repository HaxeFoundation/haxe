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

// This file is generated from mozilla\URLSearchParams.webidl. Do not edit!

package js.html;

/**
	The `URLSearchParams` interface defines utility methods to work with the query string of a URL.

	Documentation [URLSearchParams](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams>
**/
@:native("URLSearchParams")
extern class URLSearchParams {
	/** @throws DOMError */
	@:overload( function( init : haxe.DynamicAccess<String> = "") : URLSearchParams {} )
	@:overload( function( init : String = "") : URLSearchParams {} )
	function new( init : Array<Array<String>> = "" ) : Void;
	
	/**
		Appends a specified key/value pair as a new search parameter.
	**/
	function append( name : String, value : String ) : Void;
	
	/**
		Deletes the given search parameter, and its associated value, from the list of all search parameters.
	**/
	function delete( name : String ) : Void;
	
	/**
		Returns the first value associated to the given search parameter.
	**/
	function get( name : String ) : String;
	
	/**
		Returns all the values associated with a given search parameter.
	**/
	function getAll( name : String ) : Array<String>;
	
	/**
		Returns a `Boolean` indicating if such a search parameter exists.
	**/
	function has( name : String ) : Bool;
	
	/**
		Sets the value associated to a given search parameter to the given value. If there were several values, delete the others.
	**/
	function set( name : String, value : String ) : Void;
	
	/**
		Sorts all key/value pairs, if any, by their keys.
		@throws DOMError
	**/
	function sort() : Void;
	
	/**
		Returns an `Iteration_protocols` allowing to go through all key/value pairs contained in this object.
		@throws DOMError
	**/
	function entries() : URLSearchParamsIterator;
	
	/**
		Returns an `Iteration_protocols` allowing to go through all keys of the key/value pairs contained in this object.
		@throws DOMError
	**/
	function keys() : URLSearchParamsIterator;
	
	/**
		Returns an `Iteration_protocols` allowing to go through all values of the key/value pairs contained in this object.
		@throws DOMError
	**/
	function values() : URLSearchParamsIterator;
	/** @throws DOMError */
	function forEach( callback : Dynamic, ?thisArg : Dynamic ) : Void;
}
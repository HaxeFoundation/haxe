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

// This file is generated from mozilla\Headers.webidl. Do not edit!

package js.html;

/**
	The `Headers` interface of the Fetch API allows you to perform various actions on HTTP request and response headers. These actions include retrieving, setting, adding to, and removing. A `Headers` object has an associated header list, which is initially empty and consists of zero or more name and value pairs.  You can add to this using methods like `append()` (see Examples.) In all methods of this interface, header names are matched by case-insensitive byte sequence.

	Documentation [Headers](https://developer.mozilla.org/en-US/docs/Web/API/Headers) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Headers$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Headers>
**/
@:native("Headers")
extern class Headers {
	/** @throws DOMError */
	@:overload( function( ?init : Array<Array<String>>) : Headers {} )
	@:overload( function( ?init : haxe.DynamicAccess<String>) : Headers {} )
	function new( ?init : Headers ) : Void;
	
	/**
		Appends a new value onto an existing header inside a `Headers` object, or adds the header if it does not already exist.
		@throws DOMError
	**/
	function append( name : String, value : String ) : Void;
	
	/**
		Deletes a header from a `Headers` object.
		@throws DOMError
	**/
	function delete( name : String ) : Void;
	
	/**
		Returns a `ByteString` sequence of all the values of a header within a `Headers` object with a given name.
		@throws DOMError
	**/
	function get( name : String ) : String;
	
	/**
		Returns a boolean stating whether a `Headers` object contains a certain header.
		@throws DOMError
	**/
	function has( name : String ) : Bool;
	
	/**
		Sets a new value for an existing header inside a `Headers` object, or adds the header if it does not already exist.
		@throws DOMError
	**/
	function set( name : String, value : String ) : Void;
	
	/**
		Returns an `Iteration_protocols` allowing to go through all key/value pairs contained in this object.
		@throws DOMError
	**/
	function entries() : HeadersIterator;
	
	/**
		Returns an `Iteration_protocols` allowing you to go through all keys of the key/value pairs contained in this object.
		@throws DOMError
	**/
	function keys() : HeadersIterator;
	
	/**
		Returns an `Iteration_protocols` allowing you to go through all values of the key/value pairs contained in this object.
		@throws DOMError
	**/
	function values() : HeadersIterator;
	
	/**
		Executes a provided function once for each array element.
		@throws DOMError
	**/
	function forEach( callback : Dynamic, ?thisArg : Dynamic ) : Void;
}
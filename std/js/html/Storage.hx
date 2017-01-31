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

// This file is generated from mozilla\Storage.webidl. Do not edit!

package js.html;

/**
	The `Storage` interface of the Web Storage API provides access to the session storage or local storage for a particular domain, allowing you to for example add, modify or delete stored data items.

	Documentation [Storage](https://developer.mozilla.org/en-US/docs/Web/API/Storage) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Storage$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Storage>
**/
@:native("Storage")
extern class Storage
{
	
	/**
		Returns an integer representing the number of data items stored in the `Storage` object.
	**/
	var length(default,null) : Int;
	
	/** @throws DOMError */
	
	/**
		When passed a number n, this method will return the name of the nth key in the storage.
	**/
	function key( index : Int ) : String;
	/** @throws DOMError */
	
	/**
		When passed a key name, will return that key's value.
	**/
	function getItem( key : String ) : String;
	/** @throws DOMError */
	
	/**
		When passed a key name and value, will add that key to the storage, or update that key's value if it already exists.
	**/
	function setItem( key : String, value : String ) : Void;
	/** @throws DOMError */
	
	/**
		When passed a key name, will remove that key from the storage.
	**/
	function removeItem( key : String ) : Void;
	/** @throws DOMError */
	
	/**
		When invoked, will empty all keys out of the storage.
	**/
	function clear() : Void;
}
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

// This file is generated from mozilla\IDBIndex.webidl. Do not edit!

package js.html.idb;

/**
	`IDBIndex` interface of the IndexedDB API provides asynchronous access to an index in a database. An index is a kind of object store for looking up records in another object store, called the referenced object store. You use this interface to retrieve data.

	Documentation [IDBIndex](https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex>
**/
@:native("IDBIndex")
extern class Index
{
	
	/**
		The name of this index.
	**/
	var name(default,null) : String;
	
	/**
		The name of the object store referenced by this index.
	**/
	var objectStore(default,null) : ObjectStore;
	
	/**
		The key path of this index. If null, this index is not auto-populated.
	**/
	var keyPath(default,null) : Dynamic;
	
	/**
		Affects how the index behaves when the result of evaluating the index's key path yields an array. If `true`, there is one record in the index for each item in an array of keys. If `false`, then there is one record for each key that is an array.
	**/
	var multiEntry(default,null) : Bool;
	
	/**
		If `true`, this index does not allow duplicate values for a key.
	**/
	var unique(default,null) : Bool;
	
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, creates a cursor over the specified key range.
	**/
	function openCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, creates a cursor over the specified key range, as arranged by this index.
	**/
	function openKeyCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, finds either the value in the referenced object store that corresponds to the given key or the first corresponding value, if `key` is an `IDBKeyRange`.
	**/
	function get( key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, finds either the given key or the primary key, if `key` is an `IDBKeyRange`.
	**/
	function getKey( key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and in a separate thread, returns the number of records within a key range.
	**/
	function count( ?key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, in a separate thread, finds all matching values in the referenced object store that correspond to the given key or are in range, if `key` is an `IDBKeyRange`.
	**/
	function getAll( ?key : Dynamic, ?limit : Int ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, in a separate thread, finds all matching keys in the referenced object store that correspond to the given key or are in range, if `key` is an `IDBKeyRange`.
	**/
	function getAllKeys( ?key : Dynamic, ?limit : Int ) : Request;
}
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

// This file is generated from mozilla\IDBObjectStore.webidl. Do not edit!

package js.html.idb;

/**
	This example shows a variety of different uses of object stores, from updating the data structure with `IDBObjectStore.createIndex` inside an `onupgradeneeded` function, to adding a new item to our object store with `IDBObjectStore.add`. For a full working example, see our To-do Notifications app (view example live.)

	Documentation [IDBObjectStore](https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore>
**/
@:native("IDBObjectStore")
extern class ObjectStore
{
	
	/**
		The name of this object store.
	**/
	var name(default,null) : String;
	
	/**
		The key path of this object store. If this attribute is `null`, the application must provide a key for each modification operation.
	**/
	var keyPath(default,null) : Dynamic;
	
	/**
		A list of the names of indexes on objects in this object store.
	**/
	var indexNames(default,null) : js.html.DOMStringList;
	
	/**
		The `IDBTransaction` object to which this object store belongs.
	**/
	var transaction(default,null) : Transaction;
	
	/**
		The value of the auto increment flag for this object store.
	**/
	var autoIncrement(default,null) : Bool;
	
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, creates a structured clone of the `value`, and stores the cloned value in the object store. This is for updating existing records in an object store when the transaction's mode is `readwrite`.
	**/
	function put( value : Dynamic, ?key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, creates a structured clone of the `value`, and stores the cloned value in the object store. This is for adding new records to an object store.
	**/
	function add( value : Dynamic, ?key : Dynamic ) : Request;
	/** @throws DOMError */
	@:native("delete")
	function delete_( key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, returns the store object store selected by the specified key. This is for retrieving specific records from an object store.
	**/
	function get( key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Creates and immediately returns an `IDBRequest` object, and clears this object store in a separate thread. This is for deleting all current records out of an object store.
	**/
	function clear() : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, returns a new `IDBCursorWithValue` object. Used for iterating through an object store by primary key with a cursor.
	**/
	function openCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
	/** @throws DOMError */
	@:overload( function( name : String, keyPath : String, ?optionalParameters : IndexParameters ) : Index {} )
	
	/**
		Creates a new index during a version upgrade, returning a new `IDBIndex` object in the connected database.
	**/
	function createIndex( name : String, keyPath : Array<String>, ?optionalParameters : IndexParameters ) : Index;
	/** @throws DOMError */
	
	/**
		Opens an index from this object store after which it can, for example, be used to return a sequence of records sorted by that index using a cursor.
	**/
	function index( name : String ) : Index;
	/** @throws DOMError */
	
	/**
		Destroys the specified index in the connected database, used during a version upgrade.
	**/
	function deleteIndex( indexName : String ) : Void;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, returns the total number of records that match the provided key or `IDBKeyRange`. If no arguments are provided, it returns the total number of records in the store.
	**/
	function count( ?key : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object retrieves all objects in the object store matching the specified parameter or all objects in the store if no parameters are given.
	**/
	function getAll( ?key : Dynamic, ?limit : Int ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object retrieves record keys for all objects in the object store matching the specified parameter or all objects in the store if no parameters are given.
	**/
	function getAllKeys( ?key : Dynamic, ?limit : Int ) : Request;
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, returns a new `IDBCursorWithValue`. Used for iterating through an object store with a key.
	**/
	function openKeyCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
}
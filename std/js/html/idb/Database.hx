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

// This file is generated from mozilla\IDBDatabase.webidl. Do not edit!

package js.html.idb;

/**
	Inherits from: EventTarget

	Documentation [IDBDatabase](https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase>
**/
@:native("IDBDatabase")
extern class Database extends js.html.EventTarget
{
	
	/**
		A `DOMString` that contains the name of the connected database.
	**/
	var name(default,null) : String;
	
	/**
		A 64-bit integer that contains the version of the connected database. When a database is first created, this attribute is an empty string.
	**/
	var version(default,null) : Int;
	
	/**
		A `DOMStringList` that contains a list of the names of the object stores currently in the connected database.
	**/
	var objectStoreNames(default,null) : js.html.DOMStringList;
	
	/**
		Fires when access of the database is aborted.
	**/
	var onabort : haxe.Constraints.Function;
	
	/**
		Fires when access to the database fails.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		
		 Fires when a database structure change (`IDBOpenDBRequest.onupgradeneeded` event or` ``IDBFactory.deleteDatabase()` was requested elsewhere (most probably in another window/tab on the same computer). This is different from the version change transaction (see `IDBVersionChangeEvent`), but it is related.
		 
	**/
	var onversionchange : haxe.Constraints.Function;
	
	/** @throws DOMError */
	
	/**
		Creates and returns a new object store or index.
	**/
	function createObjectStore( name : String, ?optionalParameters : ObjectStoreParameters ) : ObjectStore;
	/** @throws DOMError */
	
	/**
		Destroys the object store with the given name in the connected database, along with any indexes that reference it.
	**/
	function deleteObjectStore( name : String ) : Void;
	/** @throws DOMError */
	
	/**
		Immediately returns a transaction object (`IDBTransaction`) containing the `IDBTransaction.objectStore` method, which you can use to access your object store. Runs in a separate thread.
	**/
	function transaction( storeNames : haxe.extern.EitherType<String,Array<String>>, ?mode : TransactionMode = "readonly" ) : Transaction;
	
	/**
		Returns immediately and closes the connection to a database in a separate thread.
	**/
	function close() : Void;
	/** @throws DOMError */
	function createMutableFile( name : String, ?type : String ) : Request;
}
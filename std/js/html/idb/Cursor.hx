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

// This file is generated from mozilla\IDBCursor.webidl. Do not edit!

package js.html.idb;

/**
	The `IDBCursor` interface of the IndexedDB API represents a cursor for traversing or iterating over multiple records in a database.

	Documentation [IDBCursor](https://developer.mozilla.org/en-US/docs/Web/API/IDBCursor) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBCursor$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBCursor>
**/
@:native("IDBCursor")
extern class Cursor
{
	
	/**
		Returns the `IDBObjectStore` or `IDBIndex` that the cursor is iterating. This function never returns null or throws an exception, even if the cursor is currently being iterated, has iterated past its end, or its transaction is not active.
	**/
	var source(default,null) : haxe.extern.EitherType<ObjectStore,Index>;
	
	/**
		Returns the direction of traversal of the cursor. See Constants for possible values.
	**/
	var direction(default,null) : CursorDirection;
	
	/**
		Returns the key for the record at the cursor's position. If the cursor is outside its range, this is set to `undefined`. The cursor's key can be any data type.
	**/
	var key(default,null) : Dynamic;
	
	/**
		Returns the cursor's current effective primary key. If the cursor is currently being iterated or has iterated outside its range, this is set to `undefined`. The cursor's primary key can be any data type.
	**/
	var primaryKey(default,null) : Dynamic;
	
	/** @throws DOMError */
	
	/**
		Returns an `IDBRequest` object, and, in a separate thread, updates the value at the current position of the cursor in the object store. This can be used to update specific records.
	**/
	function update( value : Dynamic ) : Request;
	/** @throws DOMError */
	
	/**
		Sets the number times a cursor should move its position forward.
	**/
	function advance( count : Int ) : Void;
	/** @throws DOMError */
	@:native("continue")
	function continue_( ?key : Dynamic ) : Void;
	/** @throws DOMError */
	@:native("delete")
	function delete_() : Request;
}
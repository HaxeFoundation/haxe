/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.idb;

/** The <code>IDBCursor</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API</a> represents a <a title="en/IndexedDB#gloss_cursor" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/Basic_Concepts_Behind_IndexedDB#gloss_cursor">cursor</a> for traversing or iterating over multiple records in a database.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBCursor">MDN</a>. */
@:native("IDBCursor")
extern class Cursor
{
	/** The cursor shows all records, including duplicates. It starts at the lower bound of the key range and moves upwards (monotonically increasing in the order of keys). */
	static inline var NEXT : Int = 0;

	/** The cursor shows all records, excluding duplicates. If multiple records exist with the same key, only the first one iterated is retrieved. It starts at the lower bound of the key range and moves upwards. */
	static inline var NEXT_NO_DUPLICATE : Int = 1;

	/** The cursor shows all records, including duplicates. It starts at the upper bound of the key range and moves downwards (monotonically decreasing in the order of keys). */
	static inline var PREV : Int = 2;

	/** The cursor shows all records, excluding duplicates. If multiple records exist with the same key, only the first one iterated is retrieved. It starts at the upper bound of the key range and moves downwards. */
	static inline var PREV_NO_DUPLICATE : Int = 3;

	/** On getting, returns the <a title="en/IndexedDB/Basic_Concepts_Behind_IndexedDB#gloss direction" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/Basic_Concepts_Behind_IndexedDB#gloss_direction">direction</a> of traversal of the cursor. See Constants for possible values. */
	var direction(default,null) : String;

	/** Returns the key for the record at the cursor's position. If the cursor is outside its range, this is <code>undefined</code>. */
	var key(default,null) : Dynamic;

	/** Returns the cursor's current effective key. If the cursor is currently being iterated or has iterated outside its range, this is <code>undefined</code>. */
	var primaryKey(default,null) : Dynamic;

	/** On getting, returns the <code>IDBObjectStore</code> or <code>IDBIndex</code> that the cursor is iterating. This function never returns null or throws an exception, even if the cursor is currently being iterated, has iterated past its end, or its transaction is not active. */
	var source(default,null) : Any;

	function advance( count : Int ) : Void;

	private inline function continue_( ?key : Key ) : Void {
		(untyped this["continue"])(key);
	}

	function delete() : Request;

	function update( value : Dynamic ) : Request;

}

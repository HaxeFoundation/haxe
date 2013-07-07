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

/** In the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API</a>, an <code>IDBDatabaseException</code> object represents exception conditions that can be encountered while performing database operations.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException">MDN</a>. */
@:native("IDBDatabaseException")
extern class DatabaseException
{
	/** A request was aborted, for example, through a call to<a title="en/IndexedDB/IDBTransaction#abort" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransaction#abort"> <code>IDBTransaction.abort</code></a>. */
	static inline var ABORT_ERR : Int = 20;

	/** A mutation operation in the transaction failed because a constraint was not satisfied. For example, an object, such as an object store or index, already exists and a request attempted to create a new one. */
	static inline var CONSTRAINT_ERR : Int = 4;

	/** Data provided to an operation does not meet requirements. */
	static inline var DATA_ERR : Int = 5;

	/** An operation was not allowed on an object. Unless the cause of the error is corrected, retrying the same operation would result in failure. */
	static inline var NON_TRANSIENT_ERR : Int = 2;

	/** <p>An operation was called on an object where it is not allowed or at a time when it is not allowed. It also occurs if a request is made on a source object that has been deleted or removed.</p> <p>More specific variants of this error includes: <code> TRANSACTION_INACTIVE_ERR</code> and <code>READ_ONLY_ERR</code>.</p> */
	static inline var NOT_ALLOWED_ERR : Int = 6;

	/** The operation failed, because the requested database object could not be found; for example, an object store did not exist but was being opened. */
	static inline var NOT_FOUND_ERR : Int = 8;

	static inline var NO_ERR : Int = 0;

	/** Either there's not enough remaining storage space or the storage quota was reached and the user declined to give more space to the database. */
	static inline var QUOTA_ERR : Int = 22;

	/** A mutation operation was attempted in a <code>READ_ONLY</code>&nbsp;transaction. */
	static inline var READ_ONLY_ERR : Int = 9;

	/** A lock for the transaction could not be obtained in a reasonable time. */
	static inline var TIMEOUT_ERR : Int = 23;

	/** A request was made against a transaction that is either not currently active or is already finished. */
	static inline var TRANSACTION_INACTIVE_ERR : Int = 7;

	/** The operation failed for reasons unrelated to the database itself, and it is not covered by any other error code; for example, a failure due to disk IO errors. */
	static inline var UNKNOWN_ERR : Int = 1;

	/** A request to open a database with a version lower than the one it already has. This can only happen with <a title="en/IndexedDB/IDBOpenDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBOpenDBRequest"><code>IDBOpenDBRequest</code></a>. */
	static inline var VER_ERR : Int = 12;

	/** The most appropriate error code for the condition. */
	var code(default,null) : Int;

	/** Error message describing the exception raised. */
	var message(default,null) : String;

	var name(default,null) : String;

	function toString() : String;

}

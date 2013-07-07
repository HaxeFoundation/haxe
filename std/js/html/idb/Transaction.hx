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

/** <p>The <code>IDBTransaction</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB&nbsp;API</a> provides a static, asynchronous transaction on a database using event handler attributes. All reading and writing of data are done within transactions. You actually use <code><a title="en/IndexedDB/IDBDatabase" rel="internal" href="/api/js/html/idb/Database">IDBDatabase</a></code> to start transactions and use <code>IDBTransaction</code> to set the mode of the transaction and access an object store and make your request. You can also use it to abort transactions.</p>
<p>Inherits from: <a title="en/DOM/EventTarget" rel="internal" href="/api/js/html/EventTarget">EventTarget</a></p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBTransaction">MDN</a>. */
@:native("IDBTransaction")
extern class Transaction extends js.html.EventTarget
{
	/** Allows data to be read but not changed.&nbsp; */
	static inline var READ_ONLY : Int = 0;

	/** Allows reading and writing of data in existing data stores to be changed. */
	static inline var READ_WRITE : Int = 1;

	/** Allows any operation to be performed, including ones that delete and create object stores and indexes. This mode is for updating the version number of transactions that were started using the <a title="en/IndexedDB/IDBDatabase#setVersion" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabase#setVersion"><code>setVersion()</code></a> method of <a title="en/IndexedDB/IDBDatabase" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabase">IDBDatabase</a> objects. Transactions of this mode cannot run concurrently with other transactions. */
	static inline var VERSION_CHANGE : Int = 2;

	/** The database connection that this transaction is associated with. */
	var db(default,null) : Database;

	var error(default,null) : js.html.DOMError;

	/** The mode for isolating access to data in the object stores that are in the scope of the transaction. For possible values, see Constants. The default value is <code><a href="#const_read_only" title="#const read only">READ_ONLY</a></code>. */
	var mode(default,null) : String;

	/** The event handler for the <code>onabort</code> event. */
	var onabort : js.html.EventListener;

	/** The event handler for the <code>oncomplete</code> event. */
	var oncomplete : js.html.EventListener;

	/** The event handler for the <code>error </code>event. */
	var onerror : js.html.EventListener;

	function abort() : Void;

	function objectStore( name : String ) : ObjectStore;

}

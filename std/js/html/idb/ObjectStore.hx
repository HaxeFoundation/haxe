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

/** The <code>IDBObjectStore</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API</a> represents an <a title="en/IndexedDB#gloss object store" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_object_store">object store</a> in a database.&nbsp;Records within an object store are sorted according to their keys. This sorting enable fast insertion, look-up, and &nbsp;ordered retrieval.&nbsp;<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBObjectStore">MDN</a>. */
@:native("IDBObjectStore")
extern class ObjectStore
{
	var autoIncrement (default,null) : Bool;

	/** A list of the names of <a title="en/IndexedDB#gloss index" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_index">indexes</a> on objects in this object store. */
	var indexNames (default,null) : js.html.DOMStringList;

	/** The <a title="en/IndexedDB#gloss key path" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_key_path">key path</a> of this object store. If this attribute is null, the application must provide a key for each modification operation. */
	var keyPath (default,null) : Any;

	/** The name of this object store. */
	var name (default,null) : String;

	var transaction (default,null) : Transaction;

	function add( value : Dynamic, ?key : Key ) : Request;

	function clear() : Request;

	/** <p>Immediately returns an <a title="IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a> object and asynchronously count the amount of objects in the object store that match the parameter, a key or a key range. If the parameter is not valid returns an exception.</p>

<div id="section_12"><span id="Parameters_2"></span><h5 class="editable">Parameters</h5>
<dl> <dt>key</dt> <dd>The key or key range that identifies the records to be counted.</dd>
</dl>
</div><div id="section_13"><span id="Returns_3"></span><h5 class="editable">Returns</h5>
<dl> <dt><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></dt> <dd>A request object on which subsequent events related to this operation are fired.</dd>
</dl>
</div><div id="section_14"><span id="Exceptions_3"></span><h5 class="editable">Exceptions</h5>
<p>This method can raise an <a title="IDBDatabaseException" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException">IDBDatabaseException</a> with the following codes:</p>
<dl> <dt><code><a href="IDBDatabaseException#DATA_ERR" rel="internal" title="en/IndexedDB/DatabaseException#DATA ERR">DATA_ERR</a></code></dt> <dd>If the object store uses in-line keys or has a key generator, and a key parameter was provided.<br> If the object store uses out-of-line keys and has no key generator, and no key parameter was provided.<br> If the object store uses in-line keys but no key generator, and the object store's key path does not yield a valid key.<br> If the key parameter was provided but does not contain a valid key.<br> If there are indexed on this object store, and using their key path on the value parameter yields a value that is not a valid key.</dd> <dt><code><a href="IDBDatabaseException#NOT_ALLOWED_ERR" rel="internal" title="en/IndexedDB/IDBDatabaseException#NOT_ALLOWED_ERR">NOT_ALLOWED_ERR</a></code></dt> <dd>The request was made on a source object that has been deleted or removed.</dd>
</dl></div> Throws DatabaseException. */
	@:overload( function( ?range : KeyRange ) :Request {} )
	function count( key : Key ) : Request;

	/** <p>Creates and returns a new index in the connected database. Note that this method must be called only from a <a title="en/IndexedDB/IDBTransaction#VERSION CHANGE" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransaction#VERSION_CHANGE"><code>VERSION_CHANGE</code></a> transaction callback.</p>
<pre>IDBIndex createIndex (
&nbsp; in DOMString name, 
&nbsp; in DOMString keyPath, 
&nbsp; in Object optionalParameters
) raises (IDBDatabaseException);

</pre>
<div id="section_16"><span id="Parameters_3"></span><h5 class="editable">Parameters</h5>
<dl> <dt>name</dt> <dd>The name of the index to create.</dd> <dt>keyPath</dt> <dd>The key path for the index to use.</dd> <dt>optionalParameters</dt> <dd> <div class="warning"><strong>Warning:</strong> The latest draft of the specification changed this to <code>IDBIndexParameters</code>, which is not yet recognized by any browser</div> <p>Options object whose attributes are optional parameters to the method. It includes the following properties:</p> <table class="standard-table"> <thead> <tr> <th scope="col" width="131">Attribute</th> <th scope="col" width="698">Description</th> </tr> </thead> <tbody> <tr> <td><code>unique</code></td> <td>If true, the index will not allow duplicate values for a single key.</td> </tr> <tr> <td><code>multientry</code></td> <td>If true, the index will add an entry in the index for each array element when the <em>keypath</em> resolves to an Array. If false, it will add one single entry containing the Array.</td> </tr> </tbody> </table> <p>Unknown parameters are ignored.</p> </dd> <dd></dd>
</dl>
</div><div id="section_17"><span id="Returns_4"></span><h5 class="editable">Returns</h5>
<dl> <dt><a title="en/IndexedDB/IDBIndex" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBIndex">IDBIndex</a></dt> <dd>The newly created index.</dd>
</dl>
</div><div id="section_18"><span id="Exceptions_4"></span><h5 class="editable">Exceptions</h5>
<p>This method can raise an <a title="en/IndexedDB/IDBDatabaseException" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException">IDBDatabaseException</a> with the following codes:</p>
<dl> <dt><code><a title="en/IndexedDB/DatabaseException#CONSTRAINT ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#CONSTRAINT_ERR">CONSTRAINT_ERR</a></code></dt> <dd>If an index with the same name (based on case-sensitive comparison) already exists in the connected database.</dd> <dt><code><a title="en/IndexedDB/DatabaseException#NOT ALLOWED ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#NOT_ALLOWED_ERR">NOT_ALLOWED_ERR</a></code></dt> <dd>If this method was not called from a <a title="en/IndexedDB/IDBTransaction#VERSION CHANGE" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransaction#VERSION_CHANGE"><code>VERSION_CHANGE</code></a> transaction callback.</dd>
</dl></div> Throws DatabaseException. */
	@:overload( function( name : String, keyPath : Array<String>, ?options : Dynamic ) :Index {} )
	function createIndex( name : String, keyPath : String, ?options : Dynamic ) : Index;

	/** <p>Immediately returns an <code><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></code> object, and removes the record specified by the given key from this object store, and any indexes that reference it, in a separate thread. If no record exists in this object store corresponding to the key, an error event is fired on the returned request object, with its <code><a title="en/IndexedDB/IDBErrorEvent#attr code" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBErrorEvent#attr_code">code</a></code> set to <code><a title="en/IndexedDB/IDBDatabaseException#NOT FOUND ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#NOT_FOUND_ERR">NOT_FOUND_ERR</a></code> and an appropriate <code><a title="en/IndexedDB/IDBErrorEvent#attr message" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBErrorEvent#attr_message">message</a></code>. If the record is successfully removed, then a success event is fired on the returned request object, using the <code><a title="en/IndexedDB/IDBTransactionEvent" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransactionEvent">IDBTransactionEvent</a></code> interface, with the <code><a title="en/IndexedDB/IDBSuccessEvent#attr result" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBSuccessEvent#attr_result">result</a></code> set to <code>undefined</code>, and <a title="en/IndexedDB/IDBTransactionEvent#attr transaction" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransactionEvent#attr_transaction">transaction</a> set to the transaction in which this object store is opened.</p>
<pre>IDBRequest delete (
  in any key
) raises (IDBDatabaseException); 
</pre>
<div id="section_20"><span id="Parameters_4"></span><h5 class="editable">Parameters</h5>
<dl> <dt>key</dt> <dd>The key to use to identify the record.</dd>
</dl>
</div><div id="section_21"><span id="Returns_5"></span><h5 class="editable">Returns</h5>
<dl> <dt><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></dt> <dd>A request object on which subsequent events related to this operation are fired.</dd>
</dl>
</div><div id="section_22"><span id="Exceptions_5"></span><h5 class="editable">Exceptions</h5>
<p>This method can raise an <a title="en/IndexedDB/IDBDatabaseException" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException">IDBDatabaseException</a> with the following codes:</p></div> Throws DatabaseException. */
	@:overload( function( keyRange : KeyRange ) :Request {} )
	function delete( key : Key ) : Request;

	function deleteIndex( name : String ) : Void;

	/** <p>Immediately returns an <a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a> object, and retrieves the requested record from the object store in a separate thread. If the operation is successful, then a success event is fired on the returned object, using the <a title="en/IndexedDB/IDBTransactionEvent" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransactionEvent">IDBTransactionEvent</a> interface, with its <code><a title="en/IndexedDB/IDBSuccessEvent#attr result" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBSuccessEvent#attr_result">result</a></code> set to the retrieved value, and <code><a title="en/IndexedDB/IDBTransactionEvent#attr transaction" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransactionEvent#attr_transaction">transaction</a></code> set to the transaction in which this object store is opened. If a record does not exist in the object store for the key parameter, then an error event is fired on the returned object, with its <code><a title="en/IndexedDB/IDBErrorEvent#attr code" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBErrorEvent#attr_code">code</a></code> set to <code><a title="en/IndexedDB/IDBDatabaseException#NOT FOUND ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#NOT_FOUND_ERR">NOT_FOUND_ERR</a></code> and an appropriate <code><a title="en/IndexedDB/IDBErrorEvent#attr message" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBErrorEvent#attr_message">message</a></code>.</p>
<p></p><div class="note"><strong>Note:</strong>&nbsp;This function produces the same result if no record with the given key exists in the database as when a record exists, but with an undefined value. To tell these situations apart, call the openCursor() method with the same key. That method provides a cursor if the record exists, and not if it does not.</div>
<p></p>

<div id="section_27"><span id="Parameters_6"></span><h5 class="editable">Parameters</h5>
<dl> <dt>key</dt> <dd>The key identifying the record to retrieve.</dd>
</dl>
</div><div id="section_28"><span id="Returns_6"></span><h5 class="editable">Returns</h5>
<dl> <dt><code><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></code></dt> <dd>A request object on which subsequent events related to this operation are fired.</dd>
</dl>
</div><div id="section_29"><span id="Exceptions_7"></span><h5 class="editable">Exceptions</h5>
<p>This method can raise an <a title="en/IndexedDB/IDBDatabaseException" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException">IDBDatabaseException</a> with the following code:</p>
<dl> <dt><code><a title="en/IndexedDB/IDBDatabaseException#DATA ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#DATA_ERR">DATA_ERR</a></code></dt> <dd>If the <code>key</code> parameter was not a valid value.</dd> <dt><code><a title="en/IndexedDB/IDBDatabaseException#TRANSACTION INACTIVE ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#TRANSACTION_INACTIVE_ERR">TRANSACTION_INACTIVE_ERR</a></code></dt> <dd>If the associated transaction is not active.</dd>
</dl>
</div> Throws DatabaseException. */
	@:overload( function( key : KeyRange ) :Request {} )
	function get( key : Key ) : Request;

	function index( name : String ) : Index;

	/** <p>Immediately returns an <a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a> object, and creates a <a title="en/IndexedDB#gloss cursor" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_cursor">cursor</a> over the records in this object store, in a separate thread. If there is even a single record that matches the <a title="en/IndexedDB#gloss key range" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_key_range">key range</a>, then a success event is fired on the returned object, with its <code><a title="en/IndexedDB/IDBSuccessEvent#attr result" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBSuccessEvent#attr_result">result</a></code> set to the <a title="en/IndexedDB/IDBCursor" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBCursor">IDBCursor</a> object for the new cursor. If no records match the key range, then a success event is fired on the returned object, with its <code><a title="en/IndexedDB/IDBSuccessEvent#attr result" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBSuccessEvent#attr_result">result</a></code> set to null.</p>
<pre>IDBRequest openCursor (
&nbsp; in optional IDBKeyRange range, 
&nbsp; in optional unsigned short direction
) raises (IDBDatabaseException);
</pre>
<div id="section_35"><span id="Parameters_8"></span><h5 class="editable">Parameters</h5>
<dl> <dt>range</dt> <dd>The key range to use as the cursor's range. If this parameter is unspecified or null, then the range includes all the records in the object store.</dd> <dt>direction</dt> <dd>The cursor's <a title="en/IndexedDB#gloss direction" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_direction">direction</a>.</dd>
</dl>
</div><div id="section_36"><span id="Returns_8"></span><h5 class="editable">Returns</h5>
<dl> <dt><code><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></code></dt> <dd>A request object on which subsequent events related to this operation are fired.</dd>
</dl>
</div><div id="section_37"><span id="Exceptions_9"></span><h5 class="editable">Exceptions</h5>
<p>This method can raise an IDBDatabaseException with the following code:</p>
<dl> <dt><code><a title="en/IndexedDB/DatabaseException#NOT ALLOWED ERR" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseException#NOT_ALLOWED_ERR">NOT_ALLOWED_ERR</a></code></dt> <dd>If this object store is not in the scope of any existing transaction on the connected database.</dd>
</dl>
</div> Throws DatabaseException. */
	@:overload( function( ?range : KeyRange, ?direction : String ) :Request {} )
	function openCursor( key : Key, ?direction : String ) : Request;

	function put( value : Dynamic, ?key : Key ) : Request;

}

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

// This file is generated from mozilla\IDBTransaction.webidl. Do not edit!

package js.html.idb;

/**
	Note that as of Firefox 40, IndexedDB transactions have relaxed durability guarantees to increase performance (see bug 1112702.) Previously in a `readwrite` transaction `IDBTransaction.oncomplete` was fired only when all data was guaranteed to have been flushed to disk. In Firefox 40+ the `complete` event is fired after the OS has been told to write the data but potentially before that data has actually been flushed to disk. The `complete` event may thus be delivered quicker than before, however, there exists a small chance that the entire transaction will be lost if the OS crashes or there is a loss of system power before the data is flushed to disk. Since such catastrophic events are rare most consumers should not need to concern themselves further.

	Documentation [IDBTransaction](https://developer.mozilla.org/en-US/docs/Web/API/IDBTransaction) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBTransaction$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBTransaction>
**/
@:native("IDBTransaction")
extern class Transaction extends js.html.EventTarget
{
	
	/**
		The mode for isolating access to data in the object stores that are in the scope of the transaction. For possible values, see the Constants section below. The default value is `readonly`.
	**/
	var mode(default,null) : TransactionMode;
	
	/**
		The database connection with which this transaction is associated.
	**/
	var db(default,null) : Database;
	
	/**
		Returns one of several types of error when there is an unsuccessful transaction. This property is `null` if the transaction is not finished, is finished and successfully committed, or was aborted with `IDBTransaction.abort` function.
	**/
	var error(default,null) : js.html.DOMError;
	
	/**
		The event handler for the `abort` event, fired when the transaction is aborted.
	**/
	var onabort : haxe.Constraints.Function;
	
	/**
		The event handler for the `complete` event, thrown when the transaction completes successfully.
	**/
	var oncomplete : haxe.Constraints.Function;
	
	/**
		The event handler for the `error` event, thrown when the transaction fails to complete.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		Returns a `DOMStringList` of the names of `IDBObjectStore` objects.
	**/
	var objectStoreNames(default,null) : js.html.DOMStringList;
	
	/** @throws DOMError */
	function objectStore( name : String ) : ObjectStore;
	/** @throws DOMError */
	function abort() : Void;
}
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

// This file is generated from mozilla\IDBRequest.webidl. Do not edit!

package js.html.idb;

/**
	The request object does not initially contain any information about the result of the operation, but once information becomes available, an event is fired on the request, and the information becomes available through the properties of the `IDBRequest` instance.

	Documentation [IDBRequest](https://developer.mozilla.org/en-US/docs/Web/API/IDBRequest) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBRequest$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBRequest>
**/
@:native("IDBRequest")
extern class Request extends js.html.EventTarget
{
	
	/**
		
		 Returns the result of the request. If the the request failed and the result is not available, an InvalidStateError exception is thrown.
		 
	**/
	var result(default,null) : Dynamic;
	
	/**
		Returns an error in the event of an unsuccessful request, indicating what went wrong.
	**/
	var error(default,null) : js.html.DOMError;
	
	/**
		The source of the request, such as an `IDBIndex` or an `IDBObjectStore`. If no source exists (such as when calling `IDBFactory.open`), it returns null.
	**/
	var source(default,null) : haxe.extern.EitherType<ObjectStore,haxe.extern.EitherType<Index,Cursor>>;
	
	/**
		The transaction for the request. This property can be null for certain requests, for example those returned from `IDBFactory.open` unless an upgrade is needed. (You're just connecting to a database, so there is no transaction to return).
	**/
	var transaction(default,null) : Transaction;
	
	/**
		The state of the request. Every request starts in the `pending` state. The state changes to `done` when the request completes successfully or when an error occurs.
	**/
	var readyState(default,null) : RequestReadyState;
	
	/**
		The event handler for the success event.
	**/
	var onsuccess : haxe.Constraints.Function;
	
	/**
		The event handler for the error event.
	**/
	var onerror : haxe.Constraints.Function;
	
}
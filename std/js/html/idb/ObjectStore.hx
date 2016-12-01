/*
 * Copyright (C)2005-2016 Haxe Foundation
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

@:native("IDBObjectStore")
extern class ObjectStore
{
	var name(default,null) : String;
	var keyPath(default,null) : Dynamic;
	var indexNames(default,null) : js.html.DOMStringList;
	var transaction(default,null) : Transaction;
	var autoIncrement(default,null) : Bool;
	
	/** @throws DOMError */
	function put( value : Dynamic, ?key : Dynamic ) : Request;
	/** @throws DOMError */
	function add( value : Dynamic, ?key : Dynamic ) : Request;
	/** @throws DOMError */
	@:native("delete")
	function delete_( key : Dynamic ) : Request;
	/** @throws DOMError */
	function get( key : Dynamic ) : Request;
	/** @throws DOMError */
	function clear() : Request;
	/** @throws DOMError */
	function openCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
	/** @throws DOMError */
	@:overload( function( name : String, keyPath : String, ?optionalParameters : IndexParameters ) : Index {} )
	function createIndex( name : String, keyPath : Array<String>, ?optionalParameters : IndexParameters ) : Index;
	/** @throws DOMError */
	function index( name : String ) : Index;
	/** @throws DOMError */
	function deleteIndex( indexName : String ) : Void;
	/** @throws DOMError */
	function count( ?key : Dynamic ) : Request;
	/** @throws DOMError */
	function getAll( ?key : Dynamic, ?limit : Int ) : Request;
	/** @throws DOMError */
	function getAllKeys( ?key : Dynamic, ?limit : Int ) : Request;
	/** @throws DOMError */
	function openKeyCursor( ?range : Dynamic, ?direction : CursorDirection = "next" ) : Request;
}
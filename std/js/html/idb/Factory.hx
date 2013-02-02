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

/** <p>The <code>IDBFactory</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB&nbsp;API</a> lets applications asynchronously access the indexed databases. The object that implements the interface is&nbsp; <code>window.indexedDB</code>. You open—that is, create and access—and delete a database with the object and not directly with <code>IDBFactory</code>.</p>
<p>This interface still has vendor prefixes, that is to say, you have to make calls with <code>mozIndexedDB.open()</code> for Firefox and <code>webkitIndexedDB.open()</code> for Chrome.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBFactory">MDN</a>. */
@:native("IDBFactory")
extern class Factory
{
	function cmp( first : Key, second : Key ) : Int;

	function deleteDatabase( name : String ) : VersionChangeRequest;

	function getDatabaseNames() : Request;

	function open( name : String, ?version : Int ) : OpenDBRequest;

}

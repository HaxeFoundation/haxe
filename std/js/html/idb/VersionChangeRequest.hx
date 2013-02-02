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

/** <div class="warning"><strong>Warning: </strong> The latest specification does not include this interface anymore as the <code>IDBDatabase.setVersion()</code> method has been removed. However, it is still implemented in not up-to-date browsers. See the compatibility table for version details.<br> The new way to do it is to use the <a title="en/IndexedDB/IDBOpenDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBOpenDBRequest"><code>IDBOpenDBRequest</code></a> interface which has now the <code>onblocked</code> handler and the newly needed <code>onupgradeneeded</code> one.</div>
<p>The <code>IDBVersionChangeRequest</code> interface the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API </a>represents a request to change the version of a database. It is used only by the <a title="en/IndexedDB/IDBDatabase#setVersion" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabase#setVersion"><code>setVersion()</code></a> method of <code><a title="en/IndexedDB/IDBDatabase" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBDatabase">IDBDatabase</a></code>.</p>
<p>Inherits from:&nbsp;<code><a title="en/IndexedDB/IDBRequest" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBRequest">IDBRequest</a></code></p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBVersionChangeRequest">MDN</a>. */
@:native("IDBVersionChangeRequest")
extern class VersionChangeRequest extends Request
{
	/** The event handler for the blocked event. */
	var onblocked : js.html.EventListener;

}

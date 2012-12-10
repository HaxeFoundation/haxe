/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/** <p>The <code>IDBVersionChangeEvent</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB&nbsp;API</a> indicates that the version of the database has changed.</p>
<p>The specification has changed and some not up-to-date browsers only support the deprecated unique attribute, <code>version</code>, from an early draft version.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBVersionChangeEvent">MDN</a>. */
@:native("IDBVersionChangeEvent")
extern class VersionChangeEvent extends js.html.Event
{
    /** <div class="warning"><strong>Warning:</strong> While this property is still implemented by not up-to-date browsers, the latest specification does replace it by the <code>oldVersion</code> and <code>newVersion</code> attributes. See compatibility table to know what browsers support them.</div> The new version of the database in a <a title="en/IndexedDB/IDBTransaction#VERSION CHANGE" rel="internal" href="https://developer.mozilla.org/en/IndexedDB/IDBTransaction#VERSION_CHANGE">VERSION_CHANGE</a> transaction. */
    var version (default,null) :String;

}

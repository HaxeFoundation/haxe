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
package js.html.sql;

/** <div><strong>DRAFT</strong>
<div>This page is not complete.</div>
</div>

<p></p>
<p>The <code>DatabaseSync</code> interface in the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API</a> represents a synchronous <a title="en/IndexedDB#gloss database connection" rel="internal" href="https://developer.mozilla.org/en/IndexedDB#gloss_database_connection">connection to a database</a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBDatabaseSync">MDN</a>. */
@:native("DatabaseSync")
extern class DatabaseSync
{
    var lastErrorMessage (default,null) :String;

    /** The version of the connected database. Has the null value when the database is first created. */
    var version (default,null) :String;

    function changeVersion (oldVersion :String, newVersion :String, ?callback_ :TransactionSyncCallback) :Void;

    function readTransaction (callback_ :TransactionSyncCallback) :Void;

    function transaction (callback_ :TransactionSyncCallback) :Void;

}

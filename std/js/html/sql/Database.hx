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
package js.html.sql;

/** <div><p>This content covers features introduced in <a rel="custom" href="https://developer.mozilla.org/en/Firefox_3_for_developers">Firefox 3</a>.</p></div>
<p></p>
<p>This document provides a high-level overview of the overall database design of the <a title="en/Places" rel="internal" href="https://developer.mozilla.org/en/Places">Places</a> system. Places is designed to be a complete replacement for the Firefox bookmarks and history systems using <a title="en/Storage" rel="internal" href="https://developer.mozilla.org/en/Storage">Storage.</a></p>
<p>View the <a class=" external" rel="external" href="http://people.mozilla.org/~dietrich/places-erd.png" title="http://people.mozilla.org/~dietrich/places-erd.png" target="_blank">schema diagram</a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/The_Places_database">MDN</a>. */
@:native("Database")
extern class Database
{
	var version(default,null) : String;

	function changeVersion( oldVersion : String, newVersion : String, ?callback_ : TransactionCallback, ?errorCallback : TransactionErrorCallback, ?successCallback : js.html.VoidCallback ) : Void;

	function readTransaction( callback_ : TransactionCallback, ?errorCallback : TransactionErrorCallback, ?successCallback : js.html.VoidCallback ) : Void;

	function transaction( callback_ : TransactionCallback, ?errorCallback : TransactionErrorCallback, ?successCallback : js.html.VoidCallback ) : Void;

}

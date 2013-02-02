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
package js.html.fs;

/** <div><strong>DRAFT</strong> <div>This page is not complete.</div>
</div>
<p>The <code>DirectoryEntry</code> interface of the <a title="en/DOM/File_API/File_System_API" rel="internal" href="https://developer.mozilla.org/en/DOM/File_API/File_System_API">FileSystem API</a> represents a directory in a file system.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/File_API/File_System_API/DirectoryEntry">MDN</a>. */
@:native("DirectoryEntry")
extern class DirectoryEntry extends Entry
{
	function createReader() : DirectoryReader;

	function getDirectory( path : String, ?options : Dynamic, ?successCallback : EntryCallback, ?errorCallback : ErrorCallback ) : Void;

	function getFile( path : String, ?options : Dynamic, ?successCallback : EntryCallback, ?errorCallback : ErrorCallback ) : Void;

	function removeRecursively( successCallback : js.html.VoidCallback, ?errorCallback : ErrorCallback ) : Void;

}

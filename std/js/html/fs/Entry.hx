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
<p>The <code>Entry</code> interface of the <a title="en/DOM/File_API/File_System_API" rel="internal" href="https://developer.mozilla.org/en/DOM/File_API/File_System_API">FileSystem API</a> represents entries in a file system. The entries can be a file&nbsp;or a <a href="https://developer.mozilla.org/en/DOM/File_API/File_system_API/DirectoryEntry" rel="internal" title="en/DOM/File_API/File_system_API/DirectoryEntry">DirectoryEntry</a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/File_API/File_System_API/Entry">MDN</a>. */
@:native("Entry")
extern class Entry
{
	/** The file system on which the entry resides. */
	var filesystem(default,null) : FileSystem;

	var fullPath(default,null) : String;

	/** The entry is a directory. */
	var isDirectory(default,null) : Bool;

	/** The entry is a file. */
	var isFile(default,null) : Bool;

	/** The name of the entry, excluding the path leading to it. */
	var name(default,null) : String;

	function copyTo( parent : DirectoryEntry, ?name : String, ?successCallback : EntryCallback, ?errorCallback : ErrorCallback ) : Void;

	function getMetadata( successCallback : MetadataCallback, ?errorCallback : ErrorCallback ) : Void;

	function getParent( ?successCallback : EntryCallback, ?errorCallback : ErrorCallback ) : Void;

	function moveTo( parent : DirectoryEntry, ?name : String, ?successCallback : EntryCallback, ?errorCallback : ErrorCallback ) : Void;

	function remove( successCallback : js.html.VoidCallback, ?errorCallback : ErrorCallback ) : Void;

	function toURL() : String;

}

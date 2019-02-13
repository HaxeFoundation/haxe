/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\FileSystemDirectoryEntry.webidl. Do not edit!

package js.html;

/**
	The `FileSystemDirectoryEntry` interface of the File and Directory Entries API represents a directory in a file system. It provides methods which make it possible to access and manipulate the files in a directory, as well as to access the entries within the directory.

	Documentation [FileSystemDirectoryEntry](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryEntry) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryEntry$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryEntry>
**/
@:native("FileSystemDirectoryEntry")
extern class FileSystemDirectoryEntry extends FileSystemEntry {
	function createReader() : FileSystemDirectoryReader;
	@:overload( function( ?path : String, ?options : FileSystemFlags, ?successCallback : haxe.Constraints.Function, ?errorCallback : haxe.Constraints.Function) : Void {} )
	@:overload( function( ?path : String, ?options : FileSystemFlags, ?successCallback : FileSystemEntryCallback, ?errorCallback : ErrorCallback) : Void {} )
	function getFile( ?path : String, ?options : FileSystemFlags, ?successCallback : FileSystemEntry -> Void, ?errorCallback : DOMException -> Void ) : Void;
	@:overload( function( ?path : String, ?options : FileSystemFlags, ?successCallback : haxe.Constraints.Function, ?errorCallback : haxe.Constraints.Function) : Void {} )
	@:overload( function( ?path : String, ?options : FileSystemFlags, ?successCallback : FileSystemEntryCallback, ?errorCallback : ErrorCallback) : Void {} )
	function getDirectory( ?path : String, ?options : FileSystemFlags, ?successCallback : FileSystemEntry -> Void, ?errorCallback : DOMException -> Void ) : Void;
}
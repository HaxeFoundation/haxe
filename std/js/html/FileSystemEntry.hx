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

// This file is generated from mozilla\FileSystemEntry.webidl. Do not edit!

package js.html;

/**
	The `FileSystemEntry` interface of the File and Directory Entries API represents a single in a file system. The entry can be a file or a directory (directories are represented by the `DirectoryEntry` interface). It includes methods for working with files—including copying, moving, removing, and reading files—as well as information about a file it points to—including the file name and its path from the root to the entry.

	Documentation [FileSystemEntry](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemEntry) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemEntry$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemEntry>
**/
@:native("FileSystemEntry")
extern class FileSystemEntry {
	
	/**
		A Boolean which is `true` if the entry represents a file. If it's not a file, this value is `false`.
	**/
	var isFile(default,null) : Bool;
	
	/**
		A `Boolean` which is `true` if the entry represents a directory; otherwise, it's `false`.
	**/
	var isDirectory(default,null) : Bool;
	
	/**
		A `USVString` containing the name of the entry (the final part of the path, after the last "/" character).
	**/
	var name(default,null) : String;
	
	/**
		A `USVString` object which provides the full, absolute path from the file system's root to the entry; it can also be thought of as a path which is relative to the root directory, prepended with a "/" character.
	**/
	var fullPath(default,null) : String;
	
	/**
		A `FileSystem` object representing the file system in which the entry is located.
	**/
	var filesystem(default,null) : FileSystem;
	
	@:overload( function( ?successCallback : haxe.Constraints.Function, ?errorCallback : haxe.Constraints.Function) : Void {} )
	@:overload( function( ?successCallback : FileSystemEntryCallback, ?errorCallback : ErrorCallback) : Void {} )
	function getParent( ?successCallback : FileSystemEntry -> Void, ?errorCallback : DOMException -> Void ) : Void;
}
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

// This file is generated from mozilla\FileSystem.webidl. Do not edit!

package js.html;

/**
	The File and Directory Entries API interface `FileSystem` is used to represent a file system. These objects can be obtained from the `filesystem` property on any file system entry. Some browsers offer additional APIs to create and manage file systems, such as Chrome's `requestFileSystem()` method.

	Documentation [FileSystem](https://developer.mozilla.org/en-US/docs/Web/API/FileSystem) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FileSystem$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FileSystem>
**/
@:native("FileSystem")
extern class FileSystem {
	
	/**
		A `USVString` representing the file system's name. This name is unique among the entire list of exposed file systems.
	**/
	var name(default,null) : String;
	
	/**
		A `FileSystemDirectoryEntry` object which represents the file system's root directory. Through this object, you can gain access to all files and directories in the file system.
	**/
	var root(default,null) : FileSystemDirectoryEntry;
	
}
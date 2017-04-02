/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package sys;

/**
	This class allows you to get information about the files and directories.

	See `sys.io.File` for the complementary file API.
**/
extern class FileSystem {

	/**
		Tells if the file or directory specified by `path` exists.

		If `path` is null, the result is unspecified.
	**/
	static function exists( path : String ) : Bool;

	/**
		Renames/moves the file or directory specified by `path` to `newPath`.

		If `path` is not a valid file system entry, or if it is not accessible,
		or if `newPath` is not accessible, an exception is thrown.

		If `path` or `newPath` are null, the result is unspecified.
	**/
	static function rename( path : String, newPath : String ) : Void;

	/**
		Returns `FileStat` information on the file or directory specified by
		`path`.

		If `path` is null, the result is unspecified.
	**/
	static function stat( path : String ) : FileStat;

	/**
		Returns the full path of the file or directory specified by `relPath`,
		which is relative to the current working directory. Symlinks will be
		followed and the path will be normalized.

		If `relPath` is null, the result is unspecified.
	**/
	static function fullPath( relPath : String ) : String;

	/**
		Returns the full path of the file or directory specified by `relPath`,
		which is relative to the current working directory. The path doesn't
		have to exist.

		If `relPath` is null, the result is unspecified.
	**/
	//@:require(haxe_ver >= 3.2)
	static function absolutePath( relPath : String ) : String;

	/**
		Tells if the file or directory specified by `path` is a directory.

		If `path` is not a valid file system entry or if its destination is not
		accessible, an exception is thrown.

		If `path` is null, the result is unspecified.
	**/
	static function isDirectory( path : String ) : Bool;

	/**
		Creates a directory specified by `path`.

		This method is recursive: The parent directories don't have to exist.

		If the directory cannot be created, an exception is thrown.

		If `path` is null, the result is unspecified.
	**/
	static function createDirectory( path : String ) : Void;

	/**
		Deletes the file specified by `path`.

		If `path` does not denote a valid file, or if that file cannot be
		deleted, an exception is thrown.

		If `path` is null, the result is unspecified.
	**/
	static function deleteFile( path : String ) : Void;

	/**
		Deletes the directory specified by `path`.

		If `path` does not denote a valid directory, or if that directory cannot
		be deleted, an exception is thrown.

		If `path` is null, the result is unspecified.
	**/
	static function deleteDirectory( path : String ) : Void;

	/**
		Returns the names of all files and directories in the directory specified
		by `path`.

		If `path` does not denote a valid directory, an exception is thrown.

		If `path` is null, the result is unspecified.
	**/
	static function readDirectory( path : String ) : Array<String>;

}

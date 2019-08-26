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

package sys;

/**
	This class provides information about files and directories.

	If `null` is passed as a file path to any function in this class, the
	result is unspecified, and may differ from target to target.

	See `sys.io.File` for the complementary file API.
**/
extern class FileSystem {
	/**
		Returns `true` if the file or directory specified by `path` exists.
	**/
	static function exists(path:String):Bool;

	/**
		Renames/moves the file or directory specified by `path` to `newPath`.

		If `path` is not a valid file system entry, or if it is not accessible,
		or if `newPath` is not accessible, an exception is thrown.
	**/
	static function rename(path:String, newPath:String):Void;

	/**
		Returns `FileStat` information for the file or directory specified by
		`path`.
	**/
	static function stat(path:String):FileStat;

	/**
		Returns the full path of the file or directory specified by `relPath`,
		which is relative to the current working directory. Symlinks will be
		followed and the path will be normalized.
	**/
	static function fullPath(relPath:String):String;

	/**
		Returns the full path of the file or directory specified by `relPath`,
		which is relative to the current working directory. The path doesn't
		have to exist.
	**/
	static function absolutePath(relPath:String):String;

	/**
		Returns `true` if the file or directory specified by `path` is a directory.

		If `path` is not a valid file system entry or if its destination is not
		accessible, an exception is thrown.
	**/
	static function isDirectory(path:String):Bool;

	/**
		Creates a directory specified by `path`.

		This method is recursive: The parent directories don't have to exist.

		If the directory cannot be created, an exception is thrown.
	**/
	static function createDirectory(path:String):Void;

	/**
		Deletes the file specified by `path`.

		If `path` does not denote a valid file, or if that file cannot be
		deleted, an exception is thrown.
	**/
	static function deleteFile(path:String):Void;

	/**
		Deletes the directory specified by `path`. Only empty directories can
		be deleted.

		If `path` does not denote a valid directory, or if that directory cannot
		be deleted, an exception is thrown.
	**/
	static function deleteDirectory(path:String):Void;

	/**
		Returns the names of all files and directories in the directory specified
		by `path`. `"."` and `".."` are not included in the output.

		If `path` does not denote a valid directory, an exception is thrown.
	**/
	static function readDirectory(path:String):Array<String>;
}

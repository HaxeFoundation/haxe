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
package sys;

/**
	This class allows you to get informations about the files and directories.
**/
extern class FileSystem {

	/**
		Tells if the given file or directory exists.
	**/
	static function exists( path : String ) : Bool;

	/**
		Rename the corresponding file or directory, allow to move it accross directories as well.
	**/
	static function rename( path : String, newpath : String ) : Void;

	/**
		Returns informations for the given file/directory.
	**/
	static function stat( path : String ) : FileStat;

	/**
		Returns the full path for the given path which is relative to the current working directory.
	**/
	static function fullPath( relpath : String ) : String;

	/**
		Tells if the given path is a directory. Throw an exception if it does not exists or is not accesible.
	**/
	static function isDirectory( path : String ) : Bool;

	/**
		Create the given directory. Not recursive : the parent directory must exists.
	**/
	static function createDirectory( path : String ) : Void;

	/**
		Delete a given file.
	**/
	static function deleteFile( path : String ) : Void;
	/**
		Delete a given directory.
	**/
	static function deleteDirectory( path : String ) : Void;

	/**
		Read all the files/directories stored into the given directory.
	**/
	static function readDirectory( path : String ) : Array<String>;

}

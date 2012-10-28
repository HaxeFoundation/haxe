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
import java.io.File;
import java.Lib;

/**
	This class allows you to get informations about the files and directories.
**/
@:coreApi
class FileSystem {

	/**
		Tells if the given file or directory exists.
	**/
	public static function exists( path : String ) : Bool
	{
		return new File(path).exists();
	}

	/**
		Rename the corresponding file or directory, allow to move it accross directories as well.
	**/
	public static function rename( path : String, newpath : String ) : Void
	{
		if (!new File(path).renameTo(new File(newpath)))
		{
			throw "Cannot rename " + path + " to " + newpath;
		}
	}

	/**
		Returns informations for the given file/directory.
	**/
	public static function stat( path : String ) : FileStat
	{
		var f = new File(path);
		if (!f.exists())
			throw "Path " + path + " doesn't exist";
		return {
			gid: 0, //java doesn't let you get this info
			uid: 0, //same
			atime: Date.now(), //same
			mtime: Date.fromTime(cast(f.lastModified(), Float)),
			ctime: Date.fromTime(cast(f.lastModified(), Float)), //same
			size: cast(f.length(), Int), //TODO: maybe change to Int64 for Haxe 3?
			dev: 0, //FIXME: not sure what that is
			ino: 0, //FIXME: not sure what that is
			nlink: 0, //FIXME: not sure what that is
			rdev: 0, //FIXME: not sure what that is
			mode: 0 //FIXME: not sure what that is
		};
	}

	/**
		Returns the full path for the given path which is relative to the current working directory.
	**/
	public static function fullPath( relpath : String ) : String
	{
		return new File(relpath).getAbsolutePath();
	}

	/**
		Tells if the given path is a directory. Throw an exception if it does not exists or is not accesible.
	**/
	public static function isDirectory( path : String ) : Bool
	{
		var f = new File(path);
		if (!f.exists())
			throw "Path " + path + " doesn't exist";
		return f.isDirectory();
	}

	/**
		Create the given directory. Not recursive : the parent directory must exists.
	**/
	public static function createDirectory( path : String ) : Void
	{
		if (!new File(path).mkdir())
			throw "Cannot create dir " + path;
	}

	/**
		Delete a given file.
	**/
	public static function deleteFile( path : String ) : Void
	{
		if (!new File(path).delete())
			throw "Cannot delete file " + path;
	}

	/**
		Delete a given directory.
	**/
	public static function deleteDirectory( path : String ) : Void
	{
		if (!new File(path).delete())
			throw "Cannot delete directory " + path;
	}

	/**
		Read all the files/directories stored into the given directory.
	**/
	public static function readDirectory( path : String ) : Array<String>
	{
		var f = new File(path);
		if (!f.exists())
			throw "Path " + path + " doesn't exist";
		return Lib.array( f.list() );
	}

}
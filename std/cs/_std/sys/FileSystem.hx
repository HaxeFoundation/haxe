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
import cs.system.io.DirectoryInfo;
import cs.system.io.File;
import cs.system.io.Directory;
import cs.system.io.FileInfo;

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
		return (File.Exists(path) || Directory.Exists(path));
	}

	/**
		Rename the corresponding file or directory, allow to move it accross directories as well.
	**/
	public static function rename( path : String, newpath : String ) : Void
	{
		Directory.Move(path, newpath);
	}

	/**
		Returns informations for the given file/directory.
	**/
	public static function stat( path : String ) : FileStat
	{
		if (File.Exists(path))
		{
			var fi = new FileInfo(path);
			return {
				gid: 0, //C# doesn't let you get this info
				uid: 0, //same
				atime: untyped Date.fromNative(fi.LastAccessTime),
				mtime: untyped Date.fromNative(fi.LastWriteTime),
				ctime: untyped Date.fromNative(fi.CreationTime),
				size: cast(fi.Length, Int), //TODO: maybe change to Int64 for Haxe 3?
				dev: 0, //FIXME: not sure what that is
				ino: 0, //FIXME: not sure what that is
				nlink: 0, //FIXME: not sure what that is
				rdev: 0, //FIXME: not sure what that is
				mode: 0 //FIXME: not sure what that is
			};
		} else if (Directory.Exists(path)) {
			var fi = new DirectoryInfo(path);
			return {
				gid: 0, //C# doesn't let you get this info
				uid: 0, //same
				atime: untyped Date.fromNative(fi.LastAccessTime),
				mtime: untyped Date.fromNative(fi.LastWriteTime),
				ctime: untyped Date.fromNative(fi.CreationTime),
				size: 0, //TODO: maybe change to Int64 for Haxe 3?
				dev: 0, //FIXME: not sure what that is
				ino: 0, //FIXME: not sure what that is
				nlink: 0, //FIXME: not sure what that is
				rdev: 0, //FIXME: not sure what that is
				mode: 0 //FIXME: not sure what that is
			};
		} else {
			throw "Path '" + path + "' doesn't exist";
		}

	}

	/**
		Returns the full path for the given path which is relative to the current working directory.
	**/
	public static function fullPath( relpath : String ) : String
	{
		return new FileInfo(relpath).FullName;
	}

	/**
		Tells if the given path is a directory. Throw an exception if it does not exists or is not accesible.
	**/
	public static function isDirectory( path : String ) : Bool
	{
		var isdir = Directory.Exists(path);
		if (isdir != File.Exists(path))
			return isdir;
		throw "Path '" + path + "' doesn't exist";
	}

	/**
		Create the given directory. Not recursive : the parent directory must exists.
	**/
	public static function createDirectory( path : String ) : Void
	{
		Directory.CreateDirectory(path);
	}

	/**
		Delete a given file.
	**/
	public static function deleteFile( path : String ) : Void
	{
		File.Delete(path);
	}

	/**
		Delete a given directory.
	**/
	public static function deleteDirectory( path : String ) : Void
	{
		Directory.Delete(path);
	}

	/**
		Read all the files/directories stored into the given directory.
	**/
	public static function readDirectory( path : String ) : Array<String>
	{
		var ret = Directory.GetFileSystemEntries(path);
		if (ret.Length > 0)
		{
			var fst = ret[0];
			var sep = "/";
			if (fst.lastIndexOf(sep) < fst.lastIndexOf("\\"))
				sep = "\\";
			for (i in 0...ret.Length)
			{
				var path = ret[i];
				ret[i] = path.substr(path.lastIndexOf(sep) + 1);
			}
		}

		return cs.Lib.array( ret );
	}

}
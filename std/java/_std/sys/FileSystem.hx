/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
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

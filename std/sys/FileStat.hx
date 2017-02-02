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
	File information, as given by `sys.FileSystem.stat`.
**/
typedef FileStat = {

	/**
		The user group id for the file's owner.
	**/
	var gid : Int;

	/** 
		The user id for the file's owner.
	**/
	var uid : Int;

	/** 
		The last access time for the file (when enabled by the file system).
	**/
	var atime : Date;

	/** 
		The last modification time for the file.
	**/
	var mtime : Date;

	/**
		The creation time for the file (not all file systems support this).
	**/
	var ctime : Date;

	/** 
		The size of the file, in bytes. 
	**/
	var size : Int;

	/** 
		The device on which stat resides.
	**/
	var dev : Int;

	/** 
		The inode number for stat.
	**/
	var ino : Int;

	/** 
		The number of hard links to stat.
	**/
	var nlink : Int;

	/** 
		The device type on which stat resides (special files only).
	**/
	var rdev : Int;

	/** 
		The permission bits of stat. The meaning of the bits is platform dependent.
	**/
	var mode : Int;
}

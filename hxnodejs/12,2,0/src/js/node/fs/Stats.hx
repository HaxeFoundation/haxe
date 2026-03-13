/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.fs;

/**
	Objects returned from `Fs.stat`, `Fs.lstat` and `Fs.fstat` and their synchronous counterparts are of this type.
**/
extern class Stats {
	var dev:Int;
	var ino:Float;
	var mode:Int;
	var nlink:Int;
	var uid:Int;
	var gid:Int;
	var rdev:Int;
	var size:Float;
	var blksize:Null<Int>;
	var blocks:Null<Float>;

	/**
		"Access Time" - Time when file data last accessed.

		Changed by the mknod(2), utimes(2), and read(2) system calls.
	**/
	var atime:Date;

	/**
		"Modified Time" - Time when file data last modified.

		Changed by the mknod(2), utimes(2), and write(2) system calls.
	**/
	var mtime:Date;

	/**
		"Change Time" - Time when file status was last changed (inode data modification).

		Changed by the chmod(2), chown(2), link(2), mknod(2), rename(2), unlink(2), utimes(2), read(2), and write(2) system calls.
	**/
	var ctime:Date;

	/**
		"Birth Time" - Time of file creation. Set once when the file is created.

		On filesystems where birthtime is not available, this field may instead hold either the ctime or 1970-01-01T00:00Z (ie, unix epoch timestamp 0).
		Note that this value may be greater than `atime` or `mtime` in this case. On Darwin and other FreeBSD variants,
		also set if the `atime` is explicitly set to an earlier value than the current birthtime using the utimes(2) system call.
	**/
	var birthtime:Date;

	function isFile():Bool;
	function isDirectory():Bool;
	function isBlockDevice():Bool;
	function isCharacterDevice():Bool;

	/**
		Only valid with `Fs.lstat`.
	**/
	function isSymbolicLink():Bool;

	function isFIFO():Bool;
	function isSocket():Bool;
}

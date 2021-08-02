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

package hl.uv;

import hl.uv.File;

/**
	Synchronous functions of `hl.uv.File`.
**/
class FileSync {
	/**
		Close file.
	**/
	@:hlNative("uv", "fs_close_wrap_sync")
	static public function close(file:File):Void {}

	/**
		Open file.
	**/
	@:hlNative("uv", "fs_open_wrap_sync")
	static public function open(path:String, flags:Array<FileOpenFlag>, mode:Int):File
		return null;

	/**
		Read from file.
	**/
	@:hlNative("uv", "fs_read_wrap_sync")
	static public function read(file:File, buf:Bytes, length:Int, offset:I64):I64
		return 0;

	/**
		Delete a name and possibly the file it refers to.
	**/
	@:hlNative("uv", "fs_unlink_wrap_sync")
	static public function unlink(path:String):Void {}

	/**
		Write to file.
	**/
	@:hlNative("uv", "fs_write_wrap_sync")
	static public function write(file:File, buf:Bytes, length:Int, offset:I64):I64
		return 0;

	/**
		Create a directory.
	**/
	@:hlNative("uv", "fs_mkdir_wrap_sync")
	static public function mkdir(path:String, mode:Int):Void {}

	/**
		Create a temporary directory.
	**/
	static public inline function mkdtemp(tpl:String):String
		return @:privateAccess String.fromUTF8(mkdtempWrap(tpl));

	@:hlNative("uv", "fs_mkdtemp_wrap_sync")
	static function mkdtempWrap(tpl:String):Bytes
		return null;

	/**
		Create a temporary file.
	**/
	static public function mkstemp(tpl:String):String
		return @:privateAccess String.fromUTF8(mkstempWrap(tpl));

	@:hlNative("uv", "fs_mkstemp_wrap_sync")
	static function mkstempWrap(tpl:String):Bytes
		return null;

	/**
		Delete a directory.
	**/
	@:hlNative("uv", "fs_rmdir_wrap_sync")
	static public function rmdir(path:String):Void {}

	/**
		Retrieves status information for the file at the given path.
	**/
	@:hlNative("uv", "fs_stat_wrap_sync")
	static public function stat(path:String):FileStat
		return null;

	/**
		Retrieves status information for the file.
	**/
	@:hlNative("uv", "fs_fstat_wrap_sync")
	static public function fstat(file:File):FileStat
		return null;

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	@:hlNative("uv", "fs_lstat_wrap_sync")
	static public function lstat(path:String):FileStat
		return null;

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	@:hlNative("uv", "fs_statfs_wrap_sync")
	static public function statFs(path:String):FileStatFs
		return null;

	/**
		Change the name or location of a file.
	**/
	@:hlNative("uv", "fs_rename_wrap_sync")
	static public function rename(path:String, newPath:String):Void {}

	/**
		Flushes file changes to storage.
	**/
	@:hlNative("uv", "fs_fsync_wrap_sync")
	static public function fsync(file:File):Void {}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	@:hlNative("uv", "fs_fdatasync_wrap_sync")
	static public function fdataSync(file:File):Void {}

	/**
		Truncate a file to a specified length.
	**/
	@:hlNative("uv", "fs_ftruncate_wrap_sync")
	static public function ftruncate(file:File, offset:I64):Void {}

	/**
		Copies a file from `path` to `newPath`.
	**/
	@:hlNative("uv", "fs_copyfile_wrap_sync")
	static public function copyFile(path:String, newPath:String, flags:Array<CopyFileFlag>):Void {}

	/**
		Transfers data between file descriptors.
	**/
	@:hlNative("uv", "fs_sendfile_wrap_sync")
	static public function sendFile(file:File, toFile:File, offset:I64, length:I64):Void {}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	@:hlNative("uv", "fs_access_wrap_sync")
	static public function access(path:String, mode:Array<FileAccessMode>):Void {}

	/**
		Changes permissions of the file at the given path.
	**/
	@:hlNative("uv", "fs_chmod_wrap_sync")
	static public function chmod(path:String, mode:Int):Void {}

	/**
		Changes permissions of the file.
	**/
	@:hlNative("uv", "fs_fchmod_wrap_sync")
	static public function fchmod(file:File, mode:Int):Void {}

	/**
		Sets timestamps of the file at the given path.
	**/
	@:hlNative("uv", "fs_utime_wrap_sync")
	static public function utime(path:String, atime:I64, mtime:I64):Void {}

	/**
		Sets timestamps of the file.
	**/
	@:hlNative("uv", "fs_futime_wrap_sync")
	static public function futime(file:File, atime:I64, mtime:I64):Void {}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	@:hlNative("uv", "fs_lutime_wrap_sync")
	static public function lutime(path:String, atime:I64, mtime:I64):Void {}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	@:hlNative("uv", "fs_link_wrap_sync")
	static public function link(path:String, link:String):Void {}

	/**
		Symlinks a file at the location given by `link`.
	**/
	@:hlNative("uv", "fs_symlink_wrap_sync")
	static public function symlink(path:String, link:String, flags:Array<FileSymlinkFlag>):Void {}

	/**
		Reads the target path of a symlink.
	**/
	@:hlNative("uv", "fs_readlink_wrap_sync")
	static public function readLink(path:String):String
		return null;

	/**
		Resolves a real absolute path to the given file.
	**/
	@:hlNative("uv", "fs_realpath_wrap_sync")
	static public function realPath(path:String):String
		return null;

	/**
		Changes owneship of the file at the given path.
	**/
	@:hlNative("uv", "fs_chown_wrap_sync")
	static public function chown(path:String, uid:Int, gid:Int):Void {}

	/**
		Changes owneship of the file.
	**/
	@:hlNative("uv", "fs_fchown_wrap_sync")
	static public function fchown(file:File, uid:Int, gid:Int):Void {}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	@:hlNative("uv", "fs_chown_wrap_sync")
	static public function lchown(path:String, uid:Int, gid:Int):Void {}
}
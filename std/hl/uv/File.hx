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

/**
	Full list of file creation flags and file status flags.
	@see open(2) man page
**/
enum abstract FileOpenFlag(Int) {
	var O_APPEND = 1;
	var O_CREAT = 2;
	var O_DIRECT = 3;
	var O_DIRECTORY = 4;
	var O_DSYNC = 5;
	var O_EXCL = 6;
	var O_EXLOCK = 7;
	var O_FILEMAP = 8;
	var O_NOATIME = 9;
	var O_NOCTTY = 10;
	var O_NOFOLLOW = 11;
	var O_NONBLOCK = 12;
	var O_RANDOM = 13;
	var O_RDONLY = 14;
	var O_RDWR = 15;
	var O_SEQUENTIAL = 16;
	var O_SHORT_LIVED = 17;
	var O_SYMLINK = 18;
	var O_SYNC = 19;
	var O_TEMPORARY = 20;
	var O_TRUNC = 21;
	var O_WRONLY = 22;
}

typedef FileStat = {
	var dev:I64; //UI64
	var mode:I64; //UI64
	var nlink:I64; //UI64
	var uid:I64; //UI64
	var gid:I64; //UI64
	var rdev:I64; //UI64
	var ino:I64; //UI64
	var size:I64; //UI64
	var blksize:I64; //UI64
	var blocks:I64; //UI64
	var flags:I64; //UI64
	var gen:I64; //UI64
	var atim:FileStatTimeSpec;
	var mtim:FileStatTimeSpec;
	var ctim:FileStatTimeSpec;
	var birthtim:FileStatTimeSpec;
}

typedef FileStatTimeSpec = {
	var sec:I64;
	var nsec:I64;
}

typedef FileStatFs = {
	var type:UI64;
	var bsize:UI64;
	var blocks:UI64;
	var bfree:UI64;
	var bavail:UI64;
	var files:UI64;
	var ffree:UI64;
	var spare:Array<UI64>;
}

enum abstract CopyFileFlag(Int) {
	var EXCL = 1;
	var FICLONE = 2;
	var FICLONE_FORCE = 3;
}

enum abstract FileAccessFlag(Int) {
	var F_OK = 0;
	var R_OK = 1;
	var W_OK = 2;
	var X_OK = 3;
}

enum abstract FileSymlinkFlag(Int) {
	var SYMLINK_DIR = 0;
	var SYMLINK_JUNCTION = 1;
}

/**
	File system operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
abstract File(Abstract<"uv_file">) {
	/**
		Close file.
	**/
	@:hlNative("uv", "fs_close_wrap")
	public function close(loop:Loop, callback:(e:UVError)->Void):Void {}

	/**
		Open file.
	**/
	@:hlNative("uv", "fs_open_wrap")
	static public function open(loop:Loop, path:String, flags:Array<FileOpenFlag>, mode:Int, callback:(e:UVError, file:Null<File>)->Void):Void {}

	/**
		Read from file.
	**/
	@:hlNative("uv", "fs_read_wrap")
	public function read(loop:Loop, buf:Bytes, length:Int, offset:I64, callback:(e:UVError, bytesRead:I64)->Void):Void {}

	/**
		Delete a name and possibly the file it refers to.
	**/
	@:hlNative("uv", "fs_unlink_wrap")
	static public function unlink(loop:Loop, path:String, callback:(e:UVError)->Void):Void {}

	/**
		Write to file.
	**/
	@:hlNative("uv", "fs_write_wrap")
	public function write(loop:Loop, buf:Bytes, length:Int, offset:I64, callback:(e:UVError, bytesWritten:I64)->Void):Void {}

	/**
		Create a directory.
	**/
	@:hlNative("uv", "fs_mkdir_wrap")
	static public function mkdir(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):Void {}

	/**
		Create a temporary directory.
	**/
	static public inline function mkdtemp(loop:Loop, tpl:String, callback:(e:UVError, path:Null<String>)->Void):Void
		mkdtempWrap(loop, tpl, (e, p) -> callback(e, p == null ? null : @:privateAccess String.fromUTF8(p)));

	@:hlNative("uv", "fs_mkdtemp_wrap")
	static function mkdtempWrap(loop:Loop, tpl:String, callback:(e:UVError, path:Null<Bytes>)->Void):Void {}

	/**
		Create a temporary file.
	**/
	static public function mkstemp(loop:Loop, tpl:String, callback:(e:UVError, file:Null<File>, path:Null<String>)->Void):Void
		mkstempWrap(loop, tpl, (e, f, p) -> callback(e, f, p == null ? null : @:privateAccess String.fromUTF8(p)));

	@:hlNative("uv", "fs_mkstemp_wrap")
	static function mkstempWrap(loop:Loop, tpl:String, callback:(e:UVError, file:Null<File>, path:Null<Bytes>)->Void):Void {}

	/**
		Delete a directory.
	**/
	@:hlNative("uv", "fs_rmdir_wrap")
	static public function rmdir(loop:Loop, path:String, callback:(e:UVError)->Void):Void {}

	/**
		Retrieves status information for the file at the given path.
	**/
	@:hlNative("uv", "fs_stat_wrap")
	static public function stat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):Void {}

	/**
		Retrieves status information for the file.
	**/
	@:hlNative("uv", "fs_fstat_wrap")
	public function fstat(loop:Loop, callback:(e:UVError, stat:Null<FileStat>)->Void):Void {}

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	@:hlNative("uv", "fs_lstat_wrap")
	static public function lstat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):Void {}

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	@:hlNative("uv", "fs_statfs_wrap")
	static public function statFs(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStatFs>)->Void):Void {}

	/**
		Change the name or location of a file.
	**/
	@:hlNative("uv", "fs_rename_wrap")
	static public function rename(loop:Loop, path:String, newPath:String, callback:(e:UVError)->Void):Void {}

	/**
		Flushes file changes to storage.
	**/
	@:hlNative("uv", "fs_fsync_wrap")
	public function fsync(loop:Loop, callback:(e:UVError)->Void):Void {}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	@:hlNative("uv", "fs_fdatasync_wrap")
	public function fdataSync(loop:Loop, callback:(e:UVError)->Void):Void {}

	/**
		Truncate a file to a specified length.
	**/
	@:hlNative("uv", "fs_ftruncate_wrap")
	public function ftruncate(loop:Loop, offset:I64, callback:(e:UVError)->Void):Void {}

	/**
		Copies a file from `path` to `newPath`.
	**/
	@:hlNative("uv", "fs_copyfile_wrap")
	static public function copyFile(loop:Loop, path:String, newPath:String, flags:Array<CopyFileFlag>, callback:(e:UVError)->Void):Void {}

	/**
		Transfers data between file descriptors.
	**/
	@:hlNative("uv", "fs_sendfile_wrap")
	public function sendFile(loop:Loop, toFile:File, offset:I64, length:I64, callback:(e:UVError)->Void):Void {}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	@:hlNative("uv", "fs_access_wrap")
	static public function access(loop:Loop, path:String, mode:Array<FileAccessMode>, callback:(e:UVError)->Void):Void {}

	/**
		Changes permissions of the file at the given path.
	**/
	@:hlNative("uv", "fs_chmod_wrap")
	static public function chmod(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):Void {}

	/**
		Changes permissions of the file.
	**/
	@:hlNative("uv", "fs_fchmod_wrap")
	public function fchmod(loop:Loop, mode:Int, callback:(e:UVError)->Void):Void {}

	/**
		Sets timestamps of the file at the given path.
	**/
	@:hlNative("uv", "fs_utime_wrap")
	static public function utime(loop:Loop, path:String, atime:I64, mtime:I64, callback:(e:UVError)->Void):Void {}

	/**
		Sets timestamps of the file.
	**/
	@:hlNative("uv", "fs_futime_wrap")
	public function futime(loop:Loop, atime:I64, mtime:I64, callback:(e:UVError)->Void):Void {}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	@:hlNative("uv", "fs_lutime_wrap")
	static public function lutime(loop:Loop, path:String, atime:I64, mtime:I64, callback:(e:UVError)->Void):Void {}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	@:hlNative("uv", "fs_link_wrap")
	static public function link(loop:Loop, path:String, link:String, callback:(e:UVError)->Void):Void {}

	/**
		Symlinks a file at the location given by `link`.
	**/
	@:hlNative("uv", "fs_symlink_wrap")
	static public function symlink(loop:Loop, path:String, link:String, flags:Array<FileSymlinkFlag>, callback:(e:UVError)->Void):Void {}

	/**
		Reads the target path of a symlink.
	**/
	@:hlNative("uv", "fs_readlink_wrap")
	static public function readLink(loop:Loop, path:String, callback:(e:UVError, target:Null<String>)->Void):Void {}

	/**
		Resolves a real absolute path to the given file.
	**/
	@:hlNative("uv", "fs_realpath_wrap")
	static public function realPath(loop:Loop, path:String, callback:(e:UVError, real:Null<String>)->Void):Void {}

	/**
		Changes owneship of the file at the given path.
	**/
	@:hlNative("uv", "fs_chown_wrap")
	static public function chown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):Void {}

	/**
		Changes owneship of the file.
	**/
	@:hlNative("uv", "fs_fchown_wrap")
	public function fchown(loop:Loop, uid:Int, gid:Int, callback:(e:UVError)->Void):Void {}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	@:hlNative("uv", "fs_chown_wrap")
	static public function lchown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):Void {}
}
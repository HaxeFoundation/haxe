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

import hl.types.ArrayObj;
import hl.uv.File;

/**
	Synchronous file system operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
abstract FileSync(File) from File to File from UvFile to UvFile {

	static inline function simpleRequest(action:(req:FsRequest)->Int):Void {
		var req = File.createReq();
		var result = action(req);
		if(result >= 0)
			result = req.getIntResult();
		req.freeReq();
		result.resolve();
	}

	static inline function pathRequest(action:(req:FsRequest)->Int):String {
		var req = File.createReq();
		var result = action(req);
		if(result >= 0)
			result = req.getIntResult();
		var path = req.getPath();
		req.freeReq();
		result.resolve();
		return path;
	}

	static inline function statRequest(action:(req:FsRequest)->Int):FileStat {
		var req = File.createReq();
		var result = action(req);
		if(result >= 0)
			result = req.getIntResult();
		var stat = req.getStat();
		req.freeReq();
		result.resolve();
		return stat;
	}

	/**
		Close file.
	**/
	public function close():Void {
		simpleRequest(req -> UV.fs_close_with_cb(null, req.r, this, false));
	}

	/**
		Open file.
	**/
	static public function open(path:String, flags:Array<FileOpenFlag>):File {
		var req = File.createReq();
		var cFlags = 0;
		var mode = 0;
		for(flag in flags) {
			cFlags |= flag.getIndex().translate_to_sys_file_open_flag();
			switch flag {
				case O_CREAT(m): mode = m;
				case _:
			}
		}
		var result = UV.fs_open_with_cb(null, req.r, path.toUTF8(), cFlags, mode, false);
		if(result >= 0)
			result = req.getIntResult();
		req.freeReq();
		return new File(result.resolve());
	}

	/**
		Read from file.
	**/
	public function read(buffer:Bytes, length:Int, offset:I64):I64 {
		var req = File.createReq();
		var buf = buffer.alloc_buf(length);
		var result = UV.fs_read_with_cb(null, req.r, this, buf, 1, offset, false);
		if(result >= 0)
			result = req.getIntResult();
		var bytesRead = req.getResult();
		buf.free_buf();
		req.freeReq();
		result.resolve();
		return bytesRead;
	}

	/**
		Delete a name and possibly the file it refers to.
	**/
	static public function unlink(path:String):Void {
		simpleRequest(req -> UV.fs_unlink_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Write to file.
	**/
	public function write(data:Bytes, length:Int, offset:I64):I64 {
		var req = File.createReq();
		var buf = data.alloc_buf(length);
		var result = UV.fs_write_with_cb(null, req.r, this, buf, 1, offset, false);
		if(result >= 0)
			result = req.getIntResult();
		var bytesWritten = req.getResult();
		buf.free_buf();
		req.freeReq();
		return bytesWritten;
	}

	/**
		Create a directory.
	**/
	static public function mkdir(path:String, mode:Int):Void {
		simpleRequest(req -> UV.fs_mkdir_with_cb(null, req.r, path.toUTF8(), mode, false));
	}

	/**
		Create a temporary directory.
	**/
	static public function mkdtemp(tpl:String):String {
		return pathRequest(req -> UV.fs_mkdtemp_with_cb(null, req.r, tpl.toUTF8(), false));
	}

	/**
		Create a temporary file.
	**/
	static public function mkstemp(tpl:String):{file:File, path:String} {
		var req = File.createReq();
		var result = UV.fs_mkstemp_with_cb(null, req.r, tpl.toUTF8(), false);
		if(result >= 0)
			result = req.getIntResult();
		var path = req.getPath();
		var fd = req.getIntResult();
		req.freeReq();
		result.resolve();
		return {file:new File(fd), path: path}
	}

	/**
		Delete a directory.
	**/
	static public function rmdir(path:String):Void {
		simpleRequest(req -> UV.fs_rmdir_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Retrieves status information for the file at the given path.
	**/
	static public function stat(path:String):FileStat {
		return statRequest(req -> UV.fs_stat_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Retrieves status information for the file.
	**/
	public function fstat():FileStat {
		return statRequest(req -> UV.fs_fstat_with_cb(null, req.r, this, false));
	}

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	static public function lstat(path:String):FileStat {
		return statRequest(req -> UV.fs_stat_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	static public function statFs(path:String):FileStatFs {
		var req = File.createReq();
		var result = UV.fs_statfs_with_cb(null, req.r, path.toUTF8(), false);
		if(result >= 0)
			result = req.getIntResult();
		var stat = switch req.r.fs_get_ptr().pointer_to_statfs() {
			case null: null;
			case s:
				var spare = s.statfs_f_spare();
				{
					type: s.statfs_f_type(),
					bsize: s.statfs_f_bsize(),
					blocks: s.statfs_f_blocks(),
					bfree: s.statfs_f_bfree(),
					bavail: s.statfs_f_bavail(),
					files: s.statfs_f_files(),
					ffree: s.statfs_f_ffree(),
					spare: [for(i in 0... spare.length) spare[i]],
				}
		}
		req.freeReq();
		result.resolve();
		return stat;
	}

	/**
		Change the name or location of a file.
	**/
	static public function rename(path:String, newPath:String):Void {
		simpleRequest(req -> UV.fs_rename_with_cb(null, req.r, path.toUTF8(), newPath.toUTF8(), false));
	}

	/**
		Flushes file changes to storage.
	**/
	public function fsync():Void {
		simpleRequest(req -> UV.fs_fsync_with_cb(null, req.r, this, false));
	}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	public function fdataSync():Void {
		simpleRequest(req -> UV.fs_fdatasync_with_cb(null, req.r, this, false));
	}

	/**
		Truncate a file to a specified length.
	**/
	public function ftruncate(offset:I64):Void {
		simpleRequest(req -> UV.fs_ftruncate_with_cb(null, req.r, this, offset, false));
	}

	/**
		Copies a file from `path` to `newPath`.
	**/
	static public function copyFile(path:String, newPath:String, flags:Null<Array<CopyFileFlag>>):Void {
		simpleRequest(req -> {
			var cFlags = 0;
			if(flags != null)
				for(f in flags)
					cFlags |= f;
			UV.fs_copyfile_with_cb(null, req.r, path.toUTF8(), newPath.toUTF8(), cFlags, false);
		});
	}

	/**
		Transfers data between file descriptors.

		Data will be read starting at `inOffset` of the source file.

		On success returns the offset of the byte following the last byte that
		was read.
	**/
	public function sendFile(toFile:File, inOffset:I64, length:I64):I64 {
		var req = File.createReq();
		var result = UV.fs_sendfile_with_cb(null, req.r, this, toFile, inOffset, length, false);
		if(result >= 0)
			result = req.getIntResult();
		var outOffset = req.getResult();
		req.freeReq();
		result.resolve();
		return outOffset;
	}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	static public function access(path:String, mode:Array<FileAccessMode>):Void {
		simpleRequest(req -> {
			var cMode = 0;
			for(m in mode)
				cMode |= m;
			UV.fs_access_with_cb(null, req.r, path.toUTF8(), cMode, false);
		});
	}

	/**
		Changes permissions of the file at the given path.
	**/
	static public function chmod(path:String, mode:Int):Void {
		simpleRequest(req -> UV.fs_chmod_with_cb(null, req.r, path.toUTF8(), mode, false));
	}

	/**
		Changes permissions of the file.
	**/
	public function fchmod(mode:Int):Void {
		simpleRequest(req -> UV.fs_fchmod_with_cb(null, req.r, this, mode, false));
	}

	/**
		Sets timestamps of the file at the given path.
	**/
	static public function utime(path:String, atime:Float, mtime:Float):Void {
		simpleRequest(req -> UV.fs_utime_with_cb(null, req.r, path.toUTF8(), atime, mtime, false));
	}

	/**
		Sets timestamps of the file.
	**/
	public function futime(atime:Float, mtime:Float):Void {
		simpleRequest(req -> UV.fs_futime_with_cb(null, req.r, this, atime, mtime, false));
	}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	static public function lutime(path:String, atime:Float, mtime:Float):Void {
		simpleRequest(req -> UV.fs_lutime_with_cb(null, req.r, path.toUTF8(), atime, mtime, false));
	}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	static public function link(path:String, link:String):Void {
		simpleRequest(req -> UV.fs_link_with_cb(null, req.r, path.toUTF8(), link.toUTF8(), false));
	}

	/**
		Symlinks a file at the location given by `link`.
	**/
	static public function symlink(path:String, link:String, flags:Null<Array<FileSymlinkFlag>>):Void {
		simpleRequest(req -> {
			var cFlags = 0;
			if(flags != null)
				for(f in flags)
					cFlags |= f;
			UV.fs_symlink_with_cb(null, req.r, path.toUTF8(), link.toUTF8(), cFlags, false);
		});
	}

	/**
		Reads the target path of a symlink.
	**/
	static public function readLink(path:String):String {
		return pathRequest(req -> UV.fs_readlink_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Resolves a real absolute path to the given file.
	**/
	static public function realPath(path:String):String {
		return pathRequest(req -> UV.fs_realpath_with_cb(null, req.r, path.toUTF8(), false));
	}

	/**
		Changes owneship of the file at the given path.
	**/
	static public function chown(path:String, uid:Int, gid:Int):Void {
		simpleRequest(req -> UV.fs_chown_with_cb(null, req.r, path.toUTF8(), uid, gid, false));
	}

	/**
		Changes owneship of the file.
	**/
	public function fchown(uid:Int, gid:Int):Void {
		simpleRequest(req -> UV.fs_fchown_with_cb(null, req.r, this, uid, gid, false));
	}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	static public function lchown(path:String, uid:Int, gid:Int):Void {
		simpleRequest(req -> UV.fs_lchown_with_cb(null, req.r, path.toUTF8(), uid, gid, false));
	}
}
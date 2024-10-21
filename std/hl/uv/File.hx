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
import hl.uv.Request;

using hl.uv.UV;

/**
	Full list of file creation flags and file status flags.
	@see open(2) man page
**/
enum FileOpenFlag {
	O_APPEND;
	O_CREAT(mode:Int);
	O_DIRECT;
	O_DIRECTORY;
	O_DSYNC;
	O_EXCL;
	O_EXLOCK;
	O_FILEMAP;
	O_NOATIME;
	O_NOCTTY;
	O_NOFOLLOW;
	O_NONBLOCK;
	O_RANDOM;
	O_RDONLY;
	O_RDWR;
	O_SEQUENTIAL;
	O_SHORT_LIVED;
	O_SYMLINK;
	O_SYNC;
	O_TEMPORARY;
	O_TRUNC;
	O_WRONLY;
}

typedef FileStat = {
	var dev:U64;
	var mode:U64;
	var nlink:U64;
	var uid:U64;
	var gid:U64;
	var rdev:U64;
	var ino:U64;
	var size:U64;
	var blksize:U64;
	var blocks:U64;
	var flags:U64;
	var gen:U64;
	var atim:FileTimeSpec;
	var mtim:FileTimeSpec;
	var ctim:FileTimeSpec;
	var birthtim:FileTimeSpec;
}

typedef FileTimeSpec = {
	var sec:I64;
	var nsec:I64;
}

typedef FileStatFs = {
	var type:U64;
	var bsize:U64;
	var blocks:U64;
	var bfree:U64;
	var bavail:U64;
	var files:U64;
	var ffree:U64;
	var spare:Array<U64>;
}

enum abstract CopyFileFlag(Int) to Int {
	var EXCL = 1;
	var FICLONE = 2;
	var FICLONE_FORCE = 4;
}

enum abstract FileAccessMode(Int) to Int {
	var F_OK = 0;
	var X_OK = 1;
	var W_OK = 2;
	var R_OK = 4;
}

enum abstract FileSymlinkFlag(Int) to Int {
	var SYMLINK_DIR = 1;
	var SYMLINK_JUNCTION = 2;
}

enum abstract FsRequestType(Int) to Int {
	var UV_FS_UNKNOWN = -1;
	var UV_FS_CUSTOM;
	var UV_FS_OPEN;
	var UV_FS_CLOSE;
	var UV_FS_READ;
	var UV_FS_WRITE;
	var UV_FS_SENDFILE;
	var UV_FS_STAT;
	var UV_FS_LSTAT;
	var UV_FS_FSTAT;
	var UV_FS_FTRUNCATE;
	var UV_FS_UTIME;
	var UV_FS_FUTIME;
	var UV_FS_ACCESS;
	var UV_FS_CHMOD;
	var UV_FS_FCHMOD;
	var UV_FS_FSYNC;
	var UV_FS_FDATASYNC;
	var UV_FS_UNLINK;
	var UV_FS_RMDIR;
	var UV_FS_MKDIR;
	var UV_FS_MKDTEMP;
	var UV_FS_RENAME;
	var UV_FS_SCANDIR;
	var UV_FS_LINK;
	var UV_FS_SYMLINK;
	var UV_FS_READLINK;
	var UV_FS_CHOWN;
	var UV_FS_FCHOWN;
	var UV_FS_REALPATH;
	var UV_FS_COPYFILE;
	var UV_FS_LCHOWN;
	var UV_FS_OPENDIR;
	var UV_FS_READDIR;
	var UV_FS_CLOSEDIR;
	var UV_FS_MKSTEMP;
	var UV_FS_LUTIME;
}

@:allow(hl.uv)
class FsRequest extends Request<UvFsTStar> {
	@:keep var callback:()->Void;
	//to keep bytes alive untile read/write request is complete
	var data:Bytes;

	inline function getIntResult():Int {
		return r.fs_get_result().toInt();
	}

	inline function getResult():U64 {
		return r.fs_get_result();
	}

	inline function getPath():Null<String> {
		return switch r.fs_get_path() {
			case null: null;
			case p: p.fromUTF8();
		}
	}

	inline function getStat():Null<FileStat> {
		return switch r.fs_get_statbuf() {
			case null: null;
			case s: File.uvStatToHl(s);
		}
	}

	override function freeReq() {
		r.req_set_data_with_gc(null);
		r.fs_req_cleanup();
		r.free_req();
		_r = null;
	}
}

/**
	File system operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
abstract File(UvFile) from UvFile to UvFile {
	static public final stdin:File = new File(0);
	static public final stdout:File = new File(1);
	static public final stderr:File = new File(2);

	/** Synchronous operations with this file */
	public var sync(get,never):FileSync;
	inline function get_sync():FileSync return this;

	@:allow(hl.uv)
	inline function new(fd:Int)
		this = new UvFile(fd);

	static inline function uvTimespecToHl(times:UvTimespecTStar):FileTimeSpec {
		return {
			sec:times.timespec_tv_sec(),
			nsec:times.timespec_tv_nsec(),
		}
	}

	@:allow(hl.uv)
	static inline function createReq():FsRequest {
		return new FsRequest(UV.alloc_fs());
	}

	@:allow(hl.uv)
	static inline function uvStatToHl(stat:UvStatTStar):FileStat {
		return {
			dev:stat.stat_st_dev(),
			mode:stat.stat_st_mode(),
			nlink:stat.stat_st_nlink(),
			uid:stat.stat_st_uid(),
			gid:stat.stat_st_gid(),
			rdev:stat.stat_st_rdev(),
			ino:stat.stat_st_ino(),
			size:stat.stat_st_size(),
			blksize:stat.stat_st_blksize(),
			blocks:stat.stat_st_blocks(),
			flags:stat.stat_st_flags(),
			gen:stat.stat_st_gen(),
			atim:uvTimespecToHl(stat.stat_st_atim()),
			mtim:uvTimespecToHl(stat.stat_st_mtim()),
			ctim:uvTimespecToHl(stat.stat_st_ctim()),
			birthtim:uvTimespecToHl(stat.stat_st_birthtim()),
		}
	}

	static inline function simpleRequest(loop:Loop, callback:(e:UVError)->Void, action:(req:FsRequest)->Int):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = action(req);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			req.freeReq();
			callback(result.translate_uv_error());
		}
		return req;
	}

	static inline function pathRequest(loop:Loop, callback:(e:UVError, path:Null<String>)->Void, action:(req:FsRequest)->Int):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = action(req);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			var path = req.getPath();
			req.freeReq();
			switch result.translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, path);
				case e: callback(e, null);
			}
		}
		return req;
	}

	static inline function ptrPathRequest(loop:Loop, callback:(e:UVError, path:Null<String>)->Void, action:(req:FsRequest)->Int):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = action(req);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			var ptr = req.r.fs_get_ptr();
			req.freeReq();
			switch result.translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, ptr.pointer_to_bytes().fromUTF8());
				case e: callback(e, null);
			}
		}
		return req;
	}

	static inline function statRequest(loop:Loop, callback:(e:UVError, stat:Null<FileStat>)->Void, action:(req:FsRequest)->Int):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = action(req);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			var stat = req.getStat();
			req.freeReq();
			callback(result.translate_uv_error(), stat);
		}
		return req;
	}

	/**
		Close file.
	**/
	public function close(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_close_with_cb(req.r, this, true));
	}

	/**
		Open file.
	**/
	static public function open(loop:Loop, path:String, flags:Array<FileOpenFlag>, callback:(e:UVError, file:File)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var cFlags = 0;
		var mode = 0;
		for(flag in flags) {
			cFlags |= flag.getIndex().translate_to_sys_file_open_flag();
			switch flag {
				case O_CREAT(m): mode = m;
				case _:
			}
		}
		var result = loop.fs_open_with_cb(req.r, path.toUTF8(), cFlags, mode, true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			req.freeReq();
			callback(result.translate_uv_error(), new File(result));
		}
		return req;
	}

	/**
		Read from file.
	**/
	public function read(loop:Loop, buffer:Bytes, length:Int, offset:I64, callback:(e:UVError, bytesRead:I64)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var buf = buffer.alloc_buf(length);
		var result = loop.fs_read_with_cb(req.r, this, buf, 1, offset, true);
		if(result < 0) {
			buf.free_buf();
			req.freeReq();
			result.throwErr();
		}
		req.data = buffer;
		req.callback = () -> {
			var result = req.getResult();
			buf.free_buf();
			req.freeReq();
			switch result.toInt().translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, I64.ofInt(0));
			}
		}
		return req;
	}

	/**
		Delete a name and possibly the file it refers to.
	**/
	static public function unlink(loop:Loop, path:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_unlink_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Write to file.
	**/
	public function write(loop:Loop, data:Bytes, length:Int, offset:I64, callback:(e:UVError, bytesWritten:I64)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var buf = data.alloc_buf(length);
		var result = loop.fs_write_with_cb(req.r, this, buf, 1, offset, true);
		if(result < 0) {
			buf.free_buf();
			req.freeReq();
			result.throwErr();
		}
		req.data = data;
		req.callback = () -> {
			var result = req.getResult();
			buf.free_buf();
			req.freeReq();
			switch result.toInt().translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, I64.ofInt(0));
			}
		}
		return req;
	}

	/**
		Create a directory.
	**/
	static public function mkdir(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_mkdir_with_cb(req.r, path.toUTF8(), mode, true));
	}

	/**
		Create a temporary directory.
	**/
	static public function mkdtemp(loop:Loop, tpl:String, callback:(e:UVError, path:Null<String>)->Void):FsRequest {
		return pathRequest(loop, callback, req -> loop.fs_mkdtemp_with_cb(req.r, tpl.toUTF8(), true));
	}

	/**
		Create a temporary file.
	**/
	static public function mkstemp(loop:Loop, tpl:String, callback:(e:UVError, file:File, path:Null<String>)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = loop.fs_mkstemp_with_cb(req.r, tpl.toUTF8(), true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			var path = req.getPath();
			req.freeReq();
			switch result.translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, new File(result), path);
				case e: callback(e, new File(result), null);
			}
		}
		return req;
	}

	/**
		Delete a directory.
	**/
	static public function rmdir(loop:Loop, path:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_rmdir_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Retrieves status information for the file at the given path.
	**/
	static public function stat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, req -> loop.fs_stat_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Retrieves status information for the file.
	**/
	public function fstat(loop:Loop, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, req -> loop.fs_fstat_with_cb(req.r, this, true));
	}

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	static public function lstat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, req -> loop.fs_lstat_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	static public function statFs(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStatFs>)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = loop.fs_statfs_with_cb(req.r, path.toUTF8(), true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
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
			callback(result.translate_uv_error(), stat);
		}
		return req;
	}

	/**
		Change the name or location of a file.
	**/
	static public function rename(loop:Loop, path:String, newPath:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_rename_with_cb(req.r, path.toUTF8(), newPath.toUTF8(), true));
	}

	/**
		Flushes file changes to storage.
	**/
	public function fsync(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_fsync_with_cb(req.r, this, true));
	}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	public function fdataSync(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_fdatasync_with_cb(req.r, this, true));
	}

	/**
		Truncate a file to a specified length.
	**/
	public function ftruncate(loop:Loop, offset:I64, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_ftruncate_with_cb(req.r, this, offset, true));
	}

	/**
		Copies a file from `path` to `newPath`.
	**/
	static public function copyFile(loop:Loop, path:String, newPath:String, flags:Null<Array<CopyFileFlag>>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> {
			var cFlags = 0;
			if(flags != null)
				for(f in flags)
					cFlags |= f;
			loop.fs_copyfile_with_cb(req.r, path.toUTF8(), newPath.toUTF8(), cFlags, true);
		});
	}

	/**
		Transfers data between file descriptors.

		Data will be read starting at `inOffset` of the source file.
		On success `outOffset` will set to the offset of the byte following the
		last byte that was read.
	**/
	public function sendFile(loop:Loop, toFile:File, inOffset:I64, length:I64, callback:(e:UVError, outOffset:I64)->Void):FsRequest {
		loop.checkLoop();
		var req = createReq();
		var result = loop.fs_sendfile_with_cb(req.r, this, toFile, inOffset, length, true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getResult();
			req.freeReq();
			switch result.toInt().translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, I64.ofInt(0));
			}
		}
		return req;
	}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	static public function access(loop:Loop, path:String, mode:Array<FileAccessMode>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> {
			var cMode = 0;
			for(m in mode)
				cMode |= m;
			loop.fs_access_with_cb(req.r, path.toUTF8(), cMode, true);
		});
	}

	/**
		Changes permissions of the file at the given path.
	**/
	static public function chmod(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_chmod_with_cb(req.r, path.toUTF8(), mode, true));
	}

	/**
		Changes permissions of the file.
	**/
	public function fchmod(loop:Loop, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_fchmod_with_cb(req.r, this, mode, true));
	}

	/**
		Sets timestamps of the file at the given path.
	**/
	static public function utime(loop:Loop, path:String, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_utime_with_cb(req.r, path.toUTF8(), atime, mtime, true));
	}

	/**
		Sets timestamps of the file.
	**/
	public function futime(loop:Loop, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_futime_with_cb(req.r, this, atime, mtime, true));
	}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	static public function lutime(loop:Loop, path:String, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_lutime_with_cb(req.r, path.toUTF8(), atime, mtime, true));
	}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	static public function link(loop:Loop, path:String, link:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_link_with_cb(req.r, path.toUTF8(), link.toUTF8(), true));
	}

	/**
		Symlinks a file at the location given by `link`.
	**/
	static public function symlink(loop:Loop, path:String, link:String, flags:Null<Array<FileSymlinkFlag>>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> {
			var cFlags = 0;
			if(flags != null)
				for(f in flags)
					cFlags |= f;
			loop.fs_symlink_with_cb(req.r, path.toUTF8(), link.toUTF8(), cFlags, true);
		});
	}

	/**
		Reads the target path of a symlink.
	**/
	static public function readLink(loop:Loop, path:String, callback:(e:UVError, target:Null<String>)->Void):FsRequest {
		return ptrPathRequest(loop, callback, req -> loop.fs_readlink_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Resolves a real absolute path to the given file.
	**/
	static public function realPath(loop:Loop, path:String, callback:(e:UVError, real:Null<String>)->Void):FsRequest {
		return ptrPathRequest(loop, callback, req -> loop.fs_realpath_with_cb(req.r, path.toUTF8(), true));
	}

	/**
		Changes owneship of the file at the given path.
	**/
	static public function chown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_chown_with_cb(req.r, path.toUTF8(), uid, gid, true));
	}

	/**
		Changes owneship of the file.
	**/
	public function fchown(loop:Loop, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_fchown_with_cb(req.r, this, uid, gid, true));
	}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	static public function lchown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, req -> loop.fs_lchown_with_cb(req.r, path.toUTF8(), uid, gid, true));
	}
}
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

package cpp.uv;

import haxe.io.Bytes;

using cpp.uv.UV;

/**
	Full list of file creation flags and file status flags.
	@see open(2) man page
**/
enum FileOpenFlag {
	APPEND;
	CREAT(mode:Int);
	DIRECT;
	DIRECTORY;
	DSYNC;
	EXCL;
	EXLOCK;
	FILEMAP;
	NOATIME;
	NOCTTY;
	NOFOLLOW;
	NONBLOCK;
	RANDOM;
	RDONLY;
	RDWR;
	SEQUENTIAL;
	SHORT_LIVED;
	SYMLINK;
	SYNC;
	TEMPORARY;
	TRUNC;
	WRONLY;
}

enum abstract CopyFileFlag(Int) to Int {
	var EXCL = 1;
	var FICLONE = 2;
	var FICLONE_FORCE = 4;
}

enum abstract FileAccessMode(Int) to Int {
	var F_OK;
	var X_OK;
	var W_OK;
	var R_OK;
}

enum abstract FileSymlinkFlag(Int) to Int {
	var SYMLINK_DIR = 1;
	var SYMLINK_JUNCTION = 2;
}

@:structInit
class FileStat {
	public var dev:UInt64;
	public var mode:UInt64;
	public var nlink:UInt64;
	public var uid:UInt64;
	public var gid:UInt64;
	public var rdev:UInt64;
	public var ino:UInt64;
	public var size:UInt64;
	public var blksize:UInt64;
	public var blocks:UInt64;
	public var flags:UInt64;
	public var gen:UInt64;
	public var atim:FileTimeSpec;
	public var mtim:FileTimeSpec;
	public var ctim:FileTimeSpec;
	public var birthtim:FileTimeSpec;

	public function toString():String {
		return '{dev:$dev, mode:$mode, nlink:$nlink, uid:$uid, gid:$gid, rdev:$rdev, ino:$ino, size:$size, blksize:$blksize, blocks:$blocks, flags:$flags, gen:$gen, atim:$atim, mtim:$mtim, ctim:$ctim, birthtim:$birthtim}';
	}
}

@:structInit
class FileTimeSpec {
	public var sec:Int64;
	public var nsec:Int64;

	public function toString():String {
		return '{sec:$sec, nsec:$nsec}';
	}
}

@:structInit
class FileStatFs {
	public var type:UInt64;
	public var bsize:UInt64;
	public var blocks:UInt64;
	public var bfree:UInt64;
	public var bavail:UInt64;
	public var files:UInt64;
	public var ffree:UInt64;
	public var spare:Array<UInt64>;

	public function toString():String {
		return '{type:$type, bsize:$bsize, blocks:$blocks, bfree:$bfree, bavail:$bavail, files:$files, ffree:$ffree, spare:$spare}';
	}
}

@:allow(cpp.uv)
@:headerCode('#include "uv.h"')
class FsRequest extends Request {
	var uvFs:RawPointer<UvFsT>;
	var callback:()->Void;
	//to keep bytes alive while waiting for a callback
	var data:Bytes;
	var buf:RawPointer<UvBufT>;

	function setupUvReq() {
		uvFs = UvFsT.create();
		uvReq = cast uvFs;
	}

	override function destructor() {
		if(buf != null)
			Stdlib.free(Pointer.fromRaw(buf));
		if(uvFs != null)
			UV.fs_req_cleanup(uvFs);
		super.destructor();
	}

	inline function getResult():SSizeT {
		return UV.fs_get_result(uvFs);
	}

	inline function getIntResult():Int {
		return UV.fs_get_result(uvFs);
	}

	inline function getPath():Null<String> {
		return switch UV.fs_get_path(uvFs) {
			case null: null;
			case p: p.toString();
		}
	}

	inline function getStat():Null<FileStat> {
		return switch UV.fs_get_statbuf(uvFs) {
			case null: null;
			case s: File.uvStatToHx(s);
		}
	}
}

/**
	Filesystem operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
@:headerCode('#include "uv.h"')
abstract File(UvFile) {
	static public final stdin:File = new File(new UvFile(0));
	static public final stdout:File = new File(new UvFile(1));
	static public final stderr:File = new File(new UvFile(2));

	/** Synchronous operations with this file */
	public var sync(get,never):FileSync;
	inline function get_sync():FileSync return new File(this);

	@:allow(cpp.uv)
	var uvFile(get,never):UvFile;
	inline function get_uvFile():UvFile
		return this;

	@:allow(cpp.uv)
	inline function new(uv:UvFile)
		this = uv;

	static function uvTimespecToHx(times:RawConstPointer<UvTimespecT>):FileTimeSpec {
		var ptr = ConstPointer.fromRaw(times);
		return {
			sec: ptr.value.tv_sec,
			nsec: ptr.value.tv_nsec,
		}
	}

	@:allow(cpp.uv)
	static function uvStatToHx(stat:RawConstPointer<UvStatT>):FileStat {
		var ptr = ConstPointer.fromRaw(stat);
		return {
			dev: ptr.value.st_dev,
			mode: ptr.value.st_mode,
			nlink: ptr.value.st_nlink,
			uid: ptr.value.st_uid,
			gid: ptr.value.st_gid,
			rdev: ptr.value.st_rdev,
			ino: ptr.value.st_ino,
			size: ptr.value.st_size,
			blksize: ptr.value.st_blksize,
			blocks: ptr.value.st_blocks,
			flags: ptr.value.st_flags,
			gen: ptr.value.st_gen,
			atim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_atim)),
			mtim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_mtim)),
			ctim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_ctim)),
			birthtim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_birthtim)),
		}
	}

	@:allow(cpp.uv)
	static inline function uvOpenFlag(flag:FileOpenFlag):UvFsOpenFlag {
		return switch flag {
			case APPEND: UV_FS_O_APPEND;
			case CREAT(_): UV_FS_O_CREAT;
			case DIRECT: UV_FS_O_DIRECT;
			case DIRECTORY: UV_FS_O_DIRECTORY;
			case DSYNC: UV_FS_O_DSYNC;
			case EXCL: UV_FS_O_EXCL;
			case EXLOCK: UV_FS_O_EXLOCK;
			case FILEMAP: UV_FS_O_FILEMAP;
			case NOATIME: UV_FS_O_NOATIME;
			case NOCTTY: UV_FS_O_NOCTTY;
			case NOFOLLOW: UV_FS_O_NOFOLLOW;
			case NONBLOCK: UV_FS_O_NONBLOCK;
			case RANDOM: UV_FS_O_RANDOM;
			case RDONLY: UV_FS_O_RDONLY;
			case RDWR: UV_FS_O_RDWR;
			case SEQUENTIAL: UV_FS_O_SEQUENTIAL;
			case SHORT_LIVED: UV_FS_O_SHORT_LIVED;
			case SYMLINK: UV_FS_O_SYMLINK;
			case SYNC: UV_FS_O_SYNC;
			case TEMPORARY: UV_FS_O_TEMPORARY;
			case TRUNC: UV_FS_O_TRUNC;
			case WRONLY: UV_FS_O_WRONLY;
		}
	}

	@:allow(cpp.uv)
	static function uvFsCb(uvFs:RawPointer<UvFsT>) {
		var req:FsRequest = cast Request.getRequest(cast uvFs);
		req.callback();
	}

	static inline function simpleRequest(loop:Loop, callback:(e:UVError)->Void, action:(req:FsRequest, cb:UvFsCb)->Int):FsRequest {
		var req = new FsRequest();
		action(req, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> callback(req.getIntResult().explain());
		// TODO: fix GC destroying request/handle objects, which don't have a reference from Haxe code.
		Sys.println('GC.run(major)');
		cpp.vm.Gc.run(true);
		Sys.println('GC.compact()');
		cpp.vm.Gc.compact();
		Sys.println('GC done');
		return req;
	}

	static inline function pathRequest(loop:Loop, callback:(e:UVError, path:Null<String>)->Void, action:(req:FsRequest, cb:UvFsCb)->Int):FsRequest {
		var req = new FsRequest();
		action(req, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> callback(req.getIntResult().explain(), req.getPath());
		return req;
	}

	static inline function statRequest(loop:Loop, callback:(e:UVError, stat:Null<FileStat>)->Void, action:(req:FsRequest, cb:UvFsCb)->Int):FsRequest {
		var req = new FsRequest();
		action(req, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> callback(req.getIntResult().explain(), req.getStat());
		return req;
	}

	/**
		Close file.
	**/
	public function close(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_close(loop.uvLoop, req.uvFs, this, cb));
	}

	/**
		Open file.
	**/
	static public function open(loop:Loop, path:String, flags:Array<FileOpenFlag>, callback:(e:UVError, file:File)->Void):FsRequest {
		var req = new FsRequest();
		var iFlags = 0;
		var mode = 0;
		for(flag in flags) {
			iFlags |= uvOpenFlag(flag);
			switch flag {
				case CREAT(m): mode = m;
				case _:
			}
		}
		UV.fs_open(loop.uvLoop, req.uvFs, path, iFlags, mode, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> {
			var result = req.getIntResult();
			callback(result.explain(), new File(new UvFile(result)));
		}
		return req;
	}

	/**
		Read from file.
	**/
	public function read(loop:Loop, buffer:Bytes, pos:Int, length:Int, offset:Int64, callback:(e:UVError, bytesRead:SSizeT)->Void):FsRequest {
		var req = new FsRequest();
		req.buf = buffer.toBuf(pos, length);
		UV.fs_read(loop.uvLoop, req.uvFs, this, req.buf, 1, offset, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.data = buffer;
		req.callback = () -> {
			var result = req.getResult();
			switch (result:Int).explain() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, 0);
			}
		}
		return req;
	}

	/**
		Delete a name and possibly the file it refers to.
	**/
	static public function unlink(loop:Loop, path:String, callback:(e:UVError)->Void):FsRequest {
		Sys.println('unlinking $path');
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_unlink(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Write to file.
	**/
	public function write(loop:Loop, data:Bytes, pos:Int, length:Int, offset:Int64, callback:(e:UVError, bytesWritten:SSizeT)->Void):FsRequest {
		var req = new FsRequest();
		req.buf = data.toBuf(pos, length);
		UV.fs_write(loop.uvLoop, req.uvFs, this, req.buf, 1, offset, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.data = data;
		req.callback = () -> {
			var result = req.getResult();
			switch (result:Int).explain() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, 0);
			}
		}
		return req;
	}

	/**
		Create a directory.
	**/
	static public function mkdir(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_mkdir(loop.uvLoop, req.uvFs, path, mode, cb));
	}

	/**
		Create a temporary directory.
	**/
	static public function mkdtemp(loop:Loop, tpl:String, callback:(e:UVError, path:Null<String>)->Void):FsRequest {
		return pathRequest(loop, callback, (req, cb) -> UV.fs_mkdtemp(loop.uvLoop, req.uvFs, tpl, cb));
	}

	/**
		Create a temporary file.
	**/
	static public function mkstemp(loop:Loop, tpl:String, callback:(e:UVError, file:File, path:Null<String>)->Void):FsRequest {
		var req = new FsRequest();
		UV.fs_mkstemp(loop.uvLoop, req.uvFs, tpl, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> {
			var result = req.getIntResult();
			callback(result.explain(), new File(new UvFile(result)), req.getPath());
		}
		return req;
	}

	/**
		Delete a directory.
	**/
	static public function rmdir(loop:Loop, path:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_rmdir(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Retrieves status information for the file at the given path.
	**/
	static public function stat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, (req, cb) -> UV.fs_stat(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Retrieves status information for the file.
	**/
	public function fstat(loop:Loop, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, (req, cb) -> UV.fs_fstat(loop.uvLoop, req.uvFs, this, cb));
	}

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	static public function lstat(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStat>)->Void):FsRequest {
		return statRequest(loop, callback, (req, cb) -> UV.fs_stat(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	static public function statFs(loop:Loop, path:String, callback:(e:UVError, stat:Null<FileStatFs>)->Void):FsRequest {
		var req = new FsRequest();
		UV.fs_statfs(loop.uvLoop, req.uvFs, path, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> {
			var stat:FileStatFs = switch (cast UV.fs_get_ptr(req.uvFs):RawPointer<UvStatfsT>) {
				case null: null;
				case s:
					var ptr = Pointer.fromRaw(s);
					var sparePtr = Pointer.fromRaw(ptr.value.f_spare);
					{
						type: ptr.value.f_type,
						bsize: ptr.value.f_bsize,
						blocks: ptr.value.f_blocks,
						bfree: ptr.value.f_bfree,
						bavail: ptr.value.f_bavail,
						files: ptr.value.f_files,
						ffree: ptr.value.f_ffree,
						spare: [for(i in 0...4) sparePtr.at(i)],
					}
			}
			callback(req.getIntResult().explain(), stat);
		}
		return req;
	}

	/**
		Change the name or location of a file.
	**/
	static public function rename(loop:Loop, path:String, newPath:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_rename(loop.uvLoop, req.uvFs, path, newPath, cb));
	}

	/**
		Flushes file changes to storage.
	**/
	public function fsync(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_fsync(loop.uvLoop, req.uvFs, this, cb));
	}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	public function fdataSync(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_fdatasync(loop.uvLoop, req.uvFs, this, cb));
	}

	/**
		Truncate a file to a specified length.
	**/
	public function ftruncate(loop:Loop, offset:Int64, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_ftruncate(loop.uvLoop, req.uvFs, this, offset, cb));
	}

	/**
		Copies a file from `path` to `newPath`.
	**/
	static public function copyFile(loop:Loop, path:String, newPath:String, flags:Null<Array<CopyFileFlag>>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> {
			var iFlags = 0;
			if(flags != null)
				for(f in flags)
					iFlags |= f;
			UV.fs_copyfile(loop.uvLoop, req.uvFs, path, newPath, iFlags, cb);
		});
	}

	/**
		Transfers data between file descriptors.
		Data will be read starting at `inOffset` of the source file.
		On success `outOffset` will set to the offset of the byte following the
		last byte that was read.
	**/
	public function sendFile(loop:Loop, toFile:File, inOffset:Int64, length:SizeT, callback:(e:UVError, outOffset:SSizeT)->Void):FsRequest {
		var req = new FsRequest();
		UV.fs_sendfile(loop.uvLoop, req.uvFs, this, toFile.uvFile, inOffset, length, Callable.fromStaticFunction(uvFsCb)).resolve();
		req.callback = () -> {
			var result = req.getResult();
			switch (result:Int).explain() {
				case UV_NOERR: callback(UV_NOERR, result);
				case e: callback(e, 0);
			}
		}
		return req;
	}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	static public function access(loop:Loop, path:String, mode:Array<FileAccessMode>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> {
			var cMode = 0;
			for(m in mode)
				cMode |= switch m {
					case F_OK: NativeFileAccessMode.F_OK;
					case X_OK: NativeFileAccessMode.X_OK;
					case W_OK: NativeFileAccessMode.W_OK;
					case R_OK: NativeFileAccessMode.R_OK;
				};
			UV.fs_access(loop.uvLoop, req.uvFs, path, cMode, cb);
		});
	}

	/**
		Changes permissions of the file at the given path.
	**/
	static public function chmod(loop:Loop, path:String, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_chmod(loop.uvLoop, req.uvFs, path, mode, cb));
	}

	/**
		Changes permissions of the file.
	**/
	public function fchmod(loop:Loop, mode:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_fchmod(loop.uvLoop, req.uvFs, this, mode, cb));
	}

	/**
		Sets timestamps of the file at the given path.
	**/
	static public function utime(loop:Loop, path:String, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_utime(loop.uvLoop, req.uvFs, path, atime, mtime, cb));
	}

	/**
		Sets timestamps of the file.
	**/
	public function futime(loop:Loop, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_futime(loop.uvLoop, req.uvFs, this, atime, mtime, cb));
	}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	static public function lutime(loop:Loop, path:String, atime:Float, mtime:Float, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_lutime(loop.uvLoop, req.uvFs, path, atime, mtime, cb));
	}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	static public function link(loop:Loop, path:String, link:String, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_link(loop.uvLoop, req.uvFs, path, link, cb));
	}

	/**
		Symlinks a file at the location given by `link`.
	**/
	static public function symlink(loop:Loop, path:String, link:String, flags:Null<Array<FileSymlinkFlag>>, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> {
			var iFlags = 0;
			if(flags != null)
				for(f in flags)
					iFlags |= f;
			UV.fs_symlink(loop.uvLoop, req.uvFs, path, link, iFlags, cb);
		});
	}

	/**
		Reads the target path of a symlink.
	**/
	static public function readLink(loop:Loop, path:String, callback:(e:UVError, target:Null<String>)->Void):FsRequest {
		return pathRequest(loop, callback, (req, cb) -> UV.fs_readlink(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Resolves a real absolute path to the given file.
	**/
	static public function realPath(loop:Loop, path:String, callback:(e:UVError, real:Null<String>)->Void):FsRequest {
		return pathRequest(loop, callback, (req, cb) -> UV.fs_realpath(loop.uvLoop, req.uvFs, path, cb));
	}

	/**
		Changes owneship of the file at the given path.
	**/
	static public function chown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_chown(loop.uvLoop, req.uvFs, path, uid, gid, cb));
	}

	/**
		Changes owneship of the file.
	**/
	public function fchown(loop:Loop, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_fchown(loop.uvLoop, req.uvFs, this, uid, gid, cb));
	}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	static public function lchown(loop:Loop, path:String, uid:Int, gid:Int, callback:(e:UVError)->Void):FsRequest {
		return simpleRequest(loop, callback, (req, cb) -> UV.fs_lchown(loop.uvLoop, req.uvFs, path, uid, gid, cb));
	}
}
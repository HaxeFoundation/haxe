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
import cpp.uv.File;

using cpp.uv.UV;

/**
	Synchronous filesystem operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
@:headerCode('#include "uv.h"')
abstract FileSync(File) from File to File {

	static inline function simpleRequest(action:(req:FsRequest)->Int):Void {
		var req = new FsRequest();
		action(req).resolve();
		req.getIntResult().resolve();
	}

	static inline function pathRequest(action:(req:FsRequest)->Int):String {
		var req = new FsRequest();
		action(req).resolve();
		req.getIntResult().resolve();
		return req.getPath();
	}

	static inline function statRequest(action:(req:FsRequest)->Int):FileStat {
		var req = new FsRequest();
		action(req).resolve();
		req.getIntResult().resolve();
		return req.getStat();
	}

	/**
		Close file.
	**/
	public function close() {
		simpleRequest(req -> UV.fs_close(null, req.uvFs, this.uvFile, null));
	}

	/**
		Open file.
	**/
	static public function open(path:String, flags:Array<FileOpenFlag>):File {
		var req = new FsRequest();
		var iFlags = 0;
		var mode = 0;
		for(flag in flags) {
			iFlags |= File.uvOpenFlag(flag);
			switch flag {
				case CREAT(m): mode = m;
				case _:
			}
		}
		UV.fs_open(null, req.uvFs, path, iFlags, mode, null).resolve();
		return new File(new UvFile(req.getIntResult().resolve()));
	}

	/**
		Read from file.
	**/
	public function read(buffer:Bytes, pos:Int, length:Int, offset:Int64):SSizeT {
		var req = new FsRequest();
		req.buf = buffer.toBuf(pos, length);
		UV.fs_read(null, req.uvFs, this.uvFile, req.buf, 1, offset, null).resolve();
		var result = req.getResult();
		(result:Int).resolve();
		return result;
	}

	/**
		Delete a name and possibly the file it refers to.
	**/
	static public function unlink(path:String) {
		simpleRequest(req -> UV.fs_unlink(null, req.uvFs, path, null));
	}

	/**
		Write to file.
	**/
	public function write(data:Bytes, pos:Int, length:Int, offset:Int64):SSizeT {
		var req = new FsRequest();
		req.buf = data.toBuf(pos, length);
		UV.fs_write(null, req.uvFs, this.uvFile, req.buf, 1, offset, null).resolve();
		var result = req.getResult();
		(result:Int).resolve();
		return result;
	}

	/**
		Create a directory.
	**/
	static public function mkdir(path:String, mode:Int) {
		simpleRequest(req -> UV.fs_mkdir(null, req.uvFs, path, mode, null));
	}

	/**
		Create a temporary directory.
	**/
	static public function mkdtemp(tpl:String):String {
		return pathRequest(req -> UV.fs_mkdtemp(null, req.uvFs, tpl, null));
	}

	/**
		Create a temporary file.
	**/
	static public function mkstemp(tpl:String):{file:File, path:String} {
		var req = new FsRequest();
		UV.fs_mkstemp(null, req.uvFs, tpl, null).resolve();
		return {file:new File(new UvFile(req.getIntResult().resolve())), path:req.getPath()}
	}

	/**
		Delete a directory.
	**/
	static public function rmdir(path:String) {
		simpleRequest(req -> UV.fs_rmdir(null, req.uvFs, path, null));
	}

	/**
		Retrieves status information for the file at the given path.
	**/
	static public function stat(path:String):FileStat {
		return statRequest(req -> UV.fs_stat(null, req.uvFs, path, null));
	}

	/**
		Retrieves status information for the file.
	**/
	public function fstat():FileStat {
		return statRequest(req -> UV.fs_fstat(null, req.uvFs, this.uvFile, null));
	}

	/**
		Retrieves status information for the file at the given path.
		If `path` is a symbolic link, then it returns information about the link
		itself, not the file that the link refers to.
	**/
	static public function lstat(path:String):FileStat {
		return statRequest(req -> UV.fs_stat(null, req.uvFs, path, null));
	}

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	static public function statFs(path:String):FileStatFs {
		var req = new FsRequest();
		UV.fs_statfs(null, req.uvFs, path, null).resolve();
		req.getIntResult().resolve();
		var s = (cast UV.fs_get_ptr(req.uvFs):RawPointer<UvStatfsT>);
		var ptr = Pointer.fromRaw(s);
		var sparePtr = Pointer.fromRaw(ptr.value.f_spare);
		return {
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

	/**
		Change the name or location of a file.
	**/
	static public function rename(path:String, newPath:String) {
		simpleRequest(req -> UV.fs_rename(null, req.uvFs, path, newPath, null));
	}

	/**
		Flushes file changes to storage.
	**/
	public function fsync() {
		simpleRequest(req -> UV.fs_fsync(null, req.uvFs, this.uvFile, null));
	}

	/**
		Flushes file changes to storage, but may omit some metadata.
	**/
	public function fdataSync() {
		simpleRequest(req -> UV.fs_fdatasync(null, req.uvFs, this.uvFile, null));
	}

	/**
		Truncate a file to a specified length.
	**/
	public function ftruncate(offset:Int64) {
		simpleRequest(req -> UV.fs_ftruncate(null, req.uvFs, this.uvFile, offset, null));
	}

	/**
		Copies a file from `path` to `newPath`.
	**/
	static public function copyFile(path:String, newPath:String, flags:Null<Array<CopyFileFlag>>) {
		simpleRequest(req -> {
			var iFlags = 0;
			if(flags != null)
				for(f in flags)
					iFlags |= f;
			UV.fs_copyfile(null, req.uvFs, path, newPath, iFlags, null);
		});
	}

	/**
		Transfers data between file descriptors.
		Data will be read starting at `inOffset` of the source file.
		On success `outOffset` will set to the offset of the byte following the
		last byte that was read.
	**/
	public function sendFile(toFile:File, inOffset:Int64, length:SizeT):SSizeT {
		var req = new FsRequest();
		UV.fs_sendfile(null, req.uvFs, this.uvFile, toFile.uvFile, inOffset, length, null).resolve();
		var result = req.getResult();
		(result:Int).resolve();
		return result;
	}

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	static public function access(path:String, mode:Array<FileAccessMode>) {
		simpleRequest(req -> {
			var cMode = 0;
			for(m in mode)
				cMode |= switch m {
					case F_OK: NativeFileAccessMode.F_OK;
					case X_OK: NativeFileAccessMode.X_OK;
					case W_OK: NativeFileAccessMode.W_OK;
					case R_OK: NativeFileAccessMode.R_OK;
				};
			UV.fs_access(null, req.uvFs, path, cMode, null);
		});
	}

	/**
		Changes permissions of the file at the given path.
	**/
	static public function chmod(path:String, mode:Int) {
		simpleRequest(req -> UV.fs_chmod(null, req.uvFs, path, mode, null));
	}

	/**
		Changes permissions of the file.
	**/
	public function fchmod(mode:Int) {
		simpleRequest(req -> UV.fs_fchmod(null, req.uvFs, this.uvFile, mode, null));
	}

	/**
		Sets timestamps of the file at the given path.
	**/
	static public function utime(path:String, atime:Float, mtime:Float) {
		simpleRequest(req -> UV.fs_utime(null, req.uvFs, path, atime, mtime, null));
	}

	/**
		Sets timestamps of the file.
	**/
	public function futime(atime:Float, mtime:Float) {
		simpleRequest(req -> UV.fs_futime(null, req.uvFs, this.uvFile, atime, mtime, null));
	}

	/**
		Sets timestamps of the file at the given path.
		If `path` is a symbolic link, then it sets timestamp of the link itself.
	**/
	static public function lutime(path:String, atime:Float, mtime:Float) {
		simpleRequest(req -> UV.fs_lutime(null, req.uvFs, path, atime, mtime, null));
	}

	/**
		Hardlinks a file at the location given by `link`.
	**/
	static public function link(path:String, link:String) {
		simpleRequest(req -> UV.fs_link(null, req.uvFs, path, link, null));
	}

	/**
		Symlinks a file at the location given by `link`.
	**/
	static public function symlink(path:String, link:String, flags:Null<Array<FileSymlinkFlag>>) {
		simpleRequest(req -> {
			var iFlags = 0;
			if(flags != null)
				for(f in flags)
					iFlags |= f;
			UV.fs_symlink(null, req.uvFs, path, link, iFlags, null);
		});
	}

	/**
		Reads the target path of a symlink.
	**/
	static public function readLink(path:String):String {
		return pathRequest(req -> UV.fs_readlink(null, req.uvFs, path, null));
	}

	/**
		Resolves a real absolute path to the given file.
	**/
	static public function realPath(path:String):String {
		return pathRequest(req -> UV.fs_realpath(null, req.uvFs, path, null));
	}

	/**
		Changes owneship of the file at the given path.
	**/
	static public function chown(path:String, uid:Int, gid:Int) {
		simpleRequest(req -> UV.fs_chown(null, req.uvFs, path, uid, gid, null));
	}

	/**
		Changes owneship of the file.
	**/
	public function fchown(uid:Int, gid:Int) {
		simpleRequest(req -> UV.fs_fchown(null, req.uvFs, this.uvFile, uid, gid, null));
	}

	/**
		Changes owneship of the file at the given path.
		If `path` is a symbolic link, the it changes ownership of the link itself.
	**/
	static public function lchown(path:String, uid:Int, gid:Int) {
		simpleRequest(req -> UV.fs_lchown(null, req.uvFs, path, uid, gid, null));
	}
}
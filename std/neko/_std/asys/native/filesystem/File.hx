package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import asys.native.filesystem.FileSystem as Fs;

@:coreApi
class File {
	public final path:FilePath;

	final handle:FileHandle;
	final deleteOnClose:Bool;

	@:allow(asys.native.filesystem.FileSystem)
	function new(f:FileHandle, path:FilePath, deleteOnClose:Bool):Void {
		this.path = path;
		this.handle = f;
		this.deleteOnClose = deleteOnClose;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		Fs.pool.runFor(
			() -> {
				try {
					if(length < 0)
						throw new FsException(CustomError('File.write(): negative length'), path);
					if(position < 0)
						throw new FsException(CustomError('File.write(): negative position'), path);
					if(offset < 0 || offset > buffer.length)
						throw new FsException(CustomError('File.write(): offset out of buffer bounds'), path);
					Fs.file_seek(handle, Int64.toInt(position), 0);
					Fs.file_write(handle, buffer.getData(), offset, length);
				} catch(e:FsException) {
					throw e;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		Fs.pool.runFor(
			() -> {
				try {
					if(length < 0)
						throw new FsException(CustomError('File.read(): negative length'), path);
					if(position < 0)
						throw new FsException(CustomError('File.read(): negative position'), path);
					if(offset < 0 || offset > buffer.length)
						throw new FsException(CustomError('File.read(): offset out of buffer bounds'), path);
					Fs.file_seek(handle, Int64.toInt(position), 0);
					Fs.file_read(handle, buffer.getData(), offset, length);
				} catch(e:FsException) {
					throw e;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function flush(callback:Callback<NoData>):Void {
		Fs.pool.runFor(
			() -> {
				try {
					Fs.file_flush(handle);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function info(callback:Callback<FileInfo>):Void {
		Fs.pool.runFor(
			() -> {
				try {
					var data = Fs.sys_stat(path);
					data.atime = Std.int(data.atime / 1000);
					data.ctime = Std.int(data.ctime / 1000);
					data.mtime = Std.int(data.mtime / 1000);
					cast data;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	public function close(callback:Callback<NoData>):Void {
		Fs.pool.runFor(
			() -> {
				try {
					Fs.file_close(handle);
					if(deleteOnClose)
						Fs.file_delete(path);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}
}
package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import asys.native.IWritable;
import asys.native.IReadable;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import lua.lib.luv.fs.FileDescriptor;
import lua.lib.luv.fs.FileSystem as Fs;
import lua.lib.luv.Idle;
import lua.NativeStringTools;
import asys.native.filesystem.FileSystem.xpcall;
import asys.native.filesystem.FileSystem.ioError;

@:coreApi
class File {
	public final path:FilePath;

	final fd:FileDescriptor;
	final deleteOnClose:Bool;

	function new(fd:FileDescriptor, path:FilePath, deleteOnClose:Bool = false):Void {
		this.fd = fd;
		this.path = path;
		this.deleteOnClose = deleteOnClose;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		var error = null;
		if(length < 0) {
			error = 'Negative length';
		} else if(position < 0) {
			error = 'Negative position';
		} else if(offset < 0 || offset > buffer.length) {
			error = 'Offset out of buffer bounds';
		}
		if(error != null) {
			failAsync(callback, CustomError(error), path);
		} else {
			var maxLength = buffer.length - offset;
			if(length > maxLength)
				length = maxLength;
			Fs.write(fd, buffer.getString(offset, length, RawNative), ofInt64(position), xpcall((e,bytesWritten) -> {
				switch ioError(e) {
					case null: callback.success(bytesWritten);
					case e: callback.fail(new FsException(e, path));
				}
			}));
		}
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		var error = null;
		if(length < 0) {
			error = 'Negative length';
		} else if(position < 0) {
			error = 'Negative position';
		} else if(offset < 0 || offset > buffer.length) {
			error = 'Offset out of buffer bounds';
		}
		if(error != null) {
			failAsync(callback, CustomError(error), path);
		} else {
			var maxLength = buffer.length - offset;
			if(length > maxLength)
				length = maxLength;
			Fs.read(fd, length, ofInt64(position), xpcall((e,data) -> switch ioError(e) {
				case null:
					var bytesRead = NativeStringTools.len(data);
					for(i in 0...bytesRead)
						buffer.set(offset + i, NativeStringTools.byte(data, i + 1));
					callback.success(bytesRead);
				case e:
					callback.fail(new FsException(e, path));
			}));
		}
	}

	public function flush(callback:Callback<NoData>):Void {
		Fs.fsync(fd, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	public function info(callback:Callback<FileInfo>):Void {
		Fs.fstat(fd, xpcall((e,stat) -> switch ioError(e) {
			case null: callback.success(stat);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		Fs.fchmod(fd, permissions, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		Fs.fchown(fd, user, group, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		Fs.ftruncate(fd, newSize, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		Fs.futime(fd, accessTime, modificationTime, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	// public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
	// 	throw new NotImplementedException();
	// }

	public function close(callback:Callback<NoData>):Void {
		if(deleteOnClose)
			Fs.unlink(path, (_,_) -> {});
		Fs.close(fd, xpcall((e,_) -> switch ioError(e) {
			case null | BadFile: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}

	//TODO: convert Int64 to lua's number without losing high bits.
	static inline function ofInt64(i64:haxe.Int64):Int {
		return Int64.toInt(i64);
	}

	function failAsync<T>(callback:Callback<T>, error:IoErrorType, path:FilePath):Void {
		var idle = new Idle();
		idle.start(() -> {
			lua.Lua.xpcall(() -> {
				idle.stop(() -> {});
				idle.close();
				callback.fail(new FsException(error, path));
			}, untyped __lua__('_hx_error'));
		});
	}
}
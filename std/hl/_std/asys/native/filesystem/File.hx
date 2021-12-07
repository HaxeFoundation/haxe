package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import sys.thread.Thread;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import hl.I64;
import hl.uv.UVError;
import hl.uv.File as LFile;
import hl.uv.Loop;
import hl.uv.Idle;
import hl.Bytes as HlBytes;
import asys.native.filesystem.FileSystem.currentLoop;
import asys.native.filesystem.FileSystem.ioError;

@:coreApi
class File {
	public final path:FilePath;

	final file:LFile;
	final deleteOnClose:Bool;

	function new(file:LFile, path:FilePath, deleteOnClose:Bool = false):Void {
		this.file = file;
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
			var pos = I64.ofInt(Int64.toInt(position)); //TODO: convert haxe.Int64 to hl.I64 directly
			var l = offset + length > buffer.length ? buffer.length - offset : length;
			file.write(currentLoop(), buffer.getData().bytes.sub(offset, l), l, pos, (e, bytesWritten) -> switch e {
				case UV_NOERR: callback.success(bytesWritten.toInt());
				case _: callback.fail(new FsException(ioError(e), path));
			});
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
			var l = offset + length > buffer.length ? buffer.length - offset : length;
			var b = new HlBytes(l);
			var pos = I64.ofInt(Int64.toInt(position)); //TODO: convert haxe.Int64 to hl.I64 directly
			file.read(currentLoop(), b, l, pos, (e, bytesRead) -> switch e {
				case UV_NOERR:
					var bytesRead = bytesRead.toInt();
					buffer.getData().bytes.blit(offset, b, 0, bytesRead);
					callback.success(bytesRead);
				case _:
					callback.fail(new FsException(ioError(e), path));
			});
		}
	}

	public function flush(callback:Callback<NoData>):Void {
		file.fsync(currentLoop(), e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	public function info(callback:Callback<FileInfo>):Void {
		file.fstat(currentLoop(), (e, stat) -> switch e {
			case UV_NOERR: callback.success(stat);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		file.fchmod(currentLoop(), permissions, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		file.fchown(currentLoop(), user, group, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		file.ftruncate(currentLoop(),I64.ofInt(newSize), e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		file.futime(currentLoop(), accessTime, modificationTime, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	// public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
	// 	throw new NotImplementedException();
	// }

	public function close(callback:Callback<NoData>):Void {
		var loop = currentLoop();
		if(deleteOnClose) {
			LFile.unlink(loop, path, _ -> {});
		}
		file.close(loop, e -> switch e {
			case UV_NOERR | UV_EBADF: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	function failAsync<T>(callback:Callback<T>, error:IoErrorType, path:FilePath):Void {
		var idle = Idle.init(currentLoop());
		idle.start(() -> {
			idle.stop();
			idle.close(() -> {});
			callback.fail(new FsException(error, path));
		});
	}
}
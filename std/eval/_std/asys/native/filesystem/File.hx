package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import sys.thread.Thread;
import asys.native.IWritable;
import asys.native.IReadable;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import eval.luv.File as LFile;
import eval.luv.Loop;
import eval.luv.Buffer;
import eval.luv.Idle;

@:coreApi
class File {
	public final path:FilePath;

	final file:LFile;
	final deleteOnClose:Bool;

	static inline function currentLoop():Loop {
		return Thread.current().events;
	}

	function new(file:LFile, path:FilePath, deleteOnClose:Bool = false):Void {
		this.file = file;
		this.path = path;
		this.deleteOnClose = deleteOnClose;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		var loop = currentLoop();
		var error = null;
		if(length < 0) {
			error = 'Negative length';
		} else if(position < 0) {
			error = 'Negative position';
		} else if(offset < 0 || offset > buffer.length) {
			error = 'Offset out of buffer bounds';
		}
		if(error != null) {
			var idle = Idle.init(loop).resolve();
			idle.start(() -> {
				idle.stop();
				idle.close(() -> {});
				callback.fail(new FsException(CustomError(error), path));
			});
		} else {
			var b = Buffer.create(offset + length > buffer.length ? buffer.length - offset : length);
			b.blitFromBytes(buffer, offset);
			file.write(currentLoop(), position, [b], null, r -> switch r {
				case Error(e): callback.fail(new FsException(e, path));
				case Ok(bytesWritten): callback.success(bytesWritten.toInt());
			});
		}
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		var loop = currentLoop();
		var error = null;
		if(length < 0) {
			error = 'Negative length';
		} else if(position < 0) {
			error = 'Negative position';
		} else if(offset < 0 || offset > buffer.length) {
			error = 'Offset out of buffer bounds';
		}
		if(error != null) {
			var idle = Idle.init(loop).resolve();
			idle.start(() -> {
				idle.stop();
				idle.close(() -> {});
				callback.fail(new FsException(CustomError(error), path));
			});
		} else {
			var b = Buffer.create(offset + length > buffer.length ? buffer.length - offset : length);
			file.read(currentLoop(), position, [b], null, r -> switch r {
				case Error(e):
					callback.fail(new FsException(e, path));
				case Ok(bytesRead):
					b.sub(0, bytesRead.toInt()).blitToBytes(buffer, offset);
					callback.success(bytesRead.toInt());
			});
		}
	}

	public function flush(callback:Callback<NoData>):Void {
		file.fsync(currentLoop(), null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function info(callback:Callback<FileInfo>):Void {
		file.fstat(currentLoop(), null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success(stat);
		});
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		file.fchmod(currentLoop(), permissions, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		file.fchown(currentLoop(), user, group, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		file.ftruncate(currentLoop(), Int64.ofInt(newSize), null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		file.futime(currentLoop(), accessTime, modificationTime, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(real): callback.success(NoData);
		});
	}

	// public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
	// 	throw new NotImplementedException();
	// }

	public function close(callback:Callback<NoData>):Void {
		var loop = currentLoop();
		if(deleteOnClose) {
			LFile.unlink(loop, path, null, _ -> {});
		}
		file.close(loop, null, r -> switch r {
			case Ok(_) | Error(UV_EBADF): callback.success(NoData);
			case Error(e): callback.fail(new FsException(e, path));
		});
	}
}
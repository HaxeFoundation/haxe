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

	static inline function currentLoop():Loop {
		return Thread.current().events;
	}

	function new(file:LFile, path:FilePath):Void {
		this.file = file;
		this.path = path;
	}

	/**
		Write up to `length` bytes from `buffer` starting at the buffer `offset`
		to the file starting at the file `position`, then invoke `callback` with
		the amount of bytes written.

		If `position` is greater than the file size then the file will be grown
		to the required size with the zero bytes before writing.

		If `position` is negative or `offset` is outside of `buffer` bounds or
		if `length` is negative, an error is passed to the `callback`.
	**/
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
					b.blitToBytes(buffer, offset);
					callback.success(bytesRead.toInt());
			});
		}
	}

	/**
		Force all buffered data to be written to disk.
	**/
	public function flush(callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Get file status information.
	**/
	public function info(callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	/**
		Set file permissions.
	**/
	public function setPermissions(mode:FilePermissions, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Set file owner and group.
	**/
	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Shrink or expand the file to `newSize` bytes.

		If the file is larger than `newSize`, the extra data is lost.
		If the file is shorter, zero bytes are used to fill the added length.
	**/
	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Change access and modification times of the file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asys.native.filesystem.FileInfo.FileStat`
	**/
	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Acquire or release a file lock for the current process.

		The `callback` is supplied with `true` if a lock was successfully acquired.

		Modes:
		- `Shared` - acquire a shared lock (usually used for reading)
		- `Exclusive` - acquire an exclusive lock (usually used for writing)
		- `Unlock` - release a lock.

		By default (`wait` is `true`) `lock` waits until a lock can be acquired.
		Pass `false` to `wait` to invoke `callback` with `false` if a lock cannot
		be acquired immediately.

		Although a lock may be released automatically on file closing, for a
		consistent cross-platform behavior it is strongly recommended to always
		release a lock manually.

		This lock is _not_ suitable for controlling access to a file by multiple threads.
	**/
	public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	public function close(callback:Callback<NoData>):Void {
		file.close(currentLoop(), null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}
}
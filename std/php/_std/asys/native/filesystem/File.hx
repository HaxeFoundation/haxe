package asys.native.filesystem;

import haxe.Int64;
import haxe.EntryPoint;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import asys.native.IWritable;
import asys.native.IReadable;
import php.Resource;
import php.Global.*;

class File implements IDuplex {

	public final path:FilePath;

	final handle:Resource;

	function new(handle:Resource, path:FilePath) {
		this.handle = handle;
		this.path = path;
	}

	/**
		Change file position pointer.
		The pointer position is used in read and write operations as the starting byte
		of reading or writing respectively.

		If `whence` is `SeekSet(offset)` set the pointer to the exact position
		specified by `offset`.
	 	If `whence` is `SeekEnd` move the pointer to the end-of-file.
		If `whence` is `SeekMove(offset)` move the pointer by `offset` bytes
		relative to the current position.
	**/
	public function seek(whence:FileSeek, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	public function getOffset(callback:Callback<Int64>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				ftell(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to get file position'), path));
				case r: callback.success(r);
			}
		});
	}

	public function isEof(callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				feof(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		throw new NotImplementedException();
	}

	public function read(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				fread(handle, length);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case (_:String) => s:
					if(strlen(s) == 0) {
						callback.success(0);
					} else {
						var bytesRead = try {
							var bytes = Bytes.ofString(s);
							buffer.blit(offset, bytes, 0, bytes.length);
							bytes.length;
						} catch(e) {
							callback.fail(new FsException(CustomError('Failed to write to buffer: ${e.message}'), path, e));
							return;
						}
						callback.success(bytesRead);
					}
			}
		});
	}

	/**
		Force all buffered data to be written to disk.
	**/
	public function flush(callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Synchronize file in-memory state with the storage device.
	**/
	public function sync(callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Get file status information.
	**/
	public function info(callback:Callback<FileInfo>) {
		throw new NotImplementedException();
	}

	/**
		Set file permissions.
	**/
	public function setPermissions(mode:FileAccessMode, callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Set file owner and group.
	**/
	public function setOwner(userId:Int, groupId:Int, callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Shrink or expand the file to `newSize` bytes.

		If the file is larger than `newSize`, the extra data is lost.
		If the file is shorter, zero bytes are used to fill the added length.
	**/
	public function resize(newSize:Int, callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Change access and modification times of the file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asys.native.filesystem.FileInfo.FileStat`
	**/
	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>) {
		throw new NotImplementedException();
	}

	/**
		Acquire or release a file lock.

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
	**/
	public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>) {
		throw new NotImplementedException();
	}

	public function close(callback:Callback<NoData>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				fclose(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(result)
				callback.success(NoData);
			else
				callback.fail(new FsException(CustomError('Failed to close a file'), path));
		});
	}
}

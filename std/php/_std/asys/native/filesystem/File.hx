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
import php.Const.*;
import php.Syntax;

class File implements IDuplex {

	public final path:FilePath;

	final handle:Resource;

	function new(handle:Resource, path:FilePath) {
		this.handle = handle;
		this.path = path;
	}

	public function seek(offset:Int64, whence:FileSeek = SeekSet, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var whence = switch whence {
					case SeekSet: SEEK_SET;
					case SeekEnd: SEEK_END;
					case SeekMove: SEEK_CUR;
				}
				var offset = if(PHP_INT_SIZE == 4) {
					Int64.toInt(offset);
				} else {
					((cast offset:{high:Int}).high << 32) | (cast offset:{low:Int}).low;
				}
				fseek(handle, offset, whence);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(result == 0)
				callback.success(NoData)
			else
				callback.fail(new FsException(CustomError('Failed to set file position'), path));
		});
	}

	public function getPosition(callback:Callback<Int64>):Void {
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

	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				if(length < 0)
					throw new php.Exception('File.write(): negative length');
				if(offset < 0 || offset >= buffer.length)
					throw new php.Exception('File.write(): offset out of bounds');
				fwrite(handle, buffer.getData().sub(offset, length));
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case _:
					callback.success(result);
			}
		});
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

	public function flush(callback:Callback<NoData>) {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				fflush(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success)
				callback.success(NoData)
			else
				callback.fail(new FsException(CustomError('Failed to flush a file'), path));
		});
	}

	public function sync(callback:Callback<NoData>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				if(function_exists('eio_fsync')) {
					Syntax.code('eio_fsync({0})', handle);
				} else {
					throw new php.Exception('asys.native.filesystem.File.sync requires Eio extension to be enabled in PHP. See https://www.php.net/manual/en/book.eio.php');
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to sync a file'), path));
				case _:
					callback.success(NoData);
			}
		});
	}

	public function info(callback:Callback<FileInfo>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				fstat(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(@:privateAccess FileSystem.phpStatToHx(result));
		});
	}

	public function setPermissions(mode:FilePermissions, callback:Callback<NoData>) {
		//PHP does not have `fchmod`
		FileSystem.setPermissions(path, mode, callback);
	}

	public function setOwner(userId:Int, groupId:Int, callback:Callback<NoData>) {
		//PHP does not have `fchown`
		FileSystem.setOwner(path, userId, groupId, callback);
	}

	public function resize(newSize:Int, callback:Callback<NoData>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var result = ftruncate(handle, newSize);
				result;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to resize file'), path));
				case _:
					callback.success(NoData);
			}
		});
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>) {
		//PHP does not have `utime` or `utimes`
		FileSystem.setTimes(path, accessTime, modificationTime, callback);
	}

	public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var mode = switch mode {
					case Exclusive: LOCK_EX;
					case Shared: LOCK_SH;
					case Unlock: LOCK_UN;
				}
				flock(handle, wait ? mode : mode | LOCK_NB);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
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

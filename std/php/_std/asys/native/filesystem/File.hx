package asys.native.filesystem;

import haxe.Int64;
import haxe.EntryPoint;
import haxe.io.Bytes;
import haxe.NoData;
import asys.native.IWritable;
import asys.native.IReadable;
import php.Resource;
import php.Global.*;
import php.Const.*;
import php.Syntax;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

class File {

	public final path:FilePath;

	final handle:Resource;
	var isClosed:Bool = false;

	function new(handle:Resource, path:FilePath) {
		this.handle = handle;
		this.path = path;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				if(length < 0)
					throw new php.Exception('File.write(): negative length');
				if(position < 0)
					throw new php.Exception('File.write(): negative position');
				if(offset < 0 || offset >= buffer.length)
					throw new php.Exception('File.write(): offset out of buffer bounds');
				if(fseek(handle, int64ToInt(position)) == 0)
					fwrite(handle, buffer.getData().sub(offset, length))
				else
					throw new php.Exception('File.write(): Failed to set file position');
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

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				if(length < 0)
					throw new php.Exception('File.read(): negative length');
				if(position < 0)
					throw new php.Exception('File.read(): negative position');
				if(offset < 0 || offset >= buffer.length)
					throw new php.Exception('File.read(): offset out of buffer bounds');
				if(fseek(handle, int64ToInt(position)) == 0)
					fread(handle, length)
				else
					throw new php.Exception('File.read(): Failed to set file position');
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

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>) {
		//PHP does not have `fchown`
		FileSystem.setOwner(path, user, group, callback);
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

	inline function int64ToInt(i64:Int64):Int {
		return if(PHP_INT_SIZE == 4) {
			Int64.toInt(i64);
		} else {
			((cast i64:{high:Int}).high << 32) | (cast i64:{low:Int}).low;
		}
	}
}

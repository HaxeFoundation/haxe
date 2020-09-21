package asys.native.filesystem;

import haxe.Int64;
import haxe.EntryPoint;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.IJobExecutor;
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
	final executor:IJobExecutor;
	var fs:Null<IFileSystem>;
	var isClosed:Bool = false;

	function new(handle:Resource, path:FilePath, executor:IJobExecutor) {
		this.handle = handle;
		this.path = path;
		this.executor = executor;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		executor.addJob(
			() -> {
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
					throw new FsException(CustomError(e.getMessage()), path);
				}
				switch result {
					case false:
						throw new FsException(CustomError('Failed to read a file'), path);
					case _:
						result;
				}
			},
			callback
		);
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		executor.addJob(
			() -> {
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
					throw new FsException(CustomError(e.getMessage()), path);
				}
				switch result {
					case false:
						throw new FsException(CustomError('Failed to read a file'), path);
					case (_:String) => s:
						if(strlen(s) == 0) {
							0;
						} else {
							var bytesRead = try {
								var bytes = Bytes.ofString(s);
								buffer.blit(offset, bytes, 0, bytes.length);
								bytes.length;
							} catch(e) {
								throw new FsException(CustomError('Failed to write to buffer: ${e.message}'), path, e);
							}
							bytesRead;
						}
				}
			},
			callback
		);
	}

	public function flush(callback:Callback<NoData>) {
		executor.addJob(
			() -> {
				var success = try {
					fflush(handle);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				if(success)
					NoData
				else
					throw new FsException(CustomError('Failed to flush a file'), path);
			},
			callback
		);
	}

	public function sync(callback:Callback<NoData>) {
		executor.addJob(
			() -> {
				var result = try {
					if(function_exists('eio_fsync')) {
						Syntax.code('eio_fsync({0})', handle);
					} else {
						throw new php.Exception('asys.native.filesystem.File.sync requires Eio extension to be enabled in PHP. See https://www.php.net/manual/en/book.eio.php');
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				switch result {
					case false:
						throw new FsException(CustomError('Failed to sync a file'), path);
					case _:
						NoData;
				}
			},
			callback
		);
	}

	public function info(callback:Callback<FileInfo>) {
		executor.addJob(
			() -> {
				var result = try {
					fstat(handle);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				@:privateAccess FileSystem.phpStatToHx(result);
			},
			callback
		);
	}

	public function setPermissions(mode:FilePermissions, callback:Callback<NoData>) {
		//PHP does not have `fchmod`
		getFs().setPermissions(path, mode, callback);
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>) {
		//PHP does not have `fchown`
		getFs().setOwner(path, user, group, callback);
	}

	public function resize(newSize:Int, callback:Callback<NoData>) {
		executor.addJob(
			() -> {
				var result = try {
					var result = ftruncate(handle, newSize);
					result;
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				switch result {
					case false:
						throw new FsException(CustomError('Failed to resize file'), path);
					case _:
						NoData;
				}
			},
			callback
		);
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>) {
		//PHP does not have `utime` or `utimes`
		getFs().setTimes(path, accessTime, modificationTime, callback);
	}

	public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>) {
		executor.addJob(
			() -> {
				var result = try {
					var mode = switch mode {
						case Exclusive: LOCK_EX;
						case Shared: LOCK_SH;
						case Unlock: LOCK_UN;
					}
					flock(handle, wait ? mode : mode | LOCK_NB);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				result;
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>) {
		executor.addJob(
			() -> {
				var result = try {
					fclose(handle);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				if(result)
					NoData;
				else
					throw new FsException(CustomError('Failed to close a file'), path);
			},
			callback
		);
	}

	inline function int64ToInt(i64:Int64):Int {
		return if(PHP_INT_SIZE == 4) {
			Int64.toInt(i64);
		} else {
			((cast i64:{high:Int}).high << 32) | (cast i64:{low:Int}).low;
		}
	}

	function getFs():IFileSystem {
		switch fs {
			case null:
				var fs = FileSystem.create(executor);
				this.fs = fs;
				return fs;
			case fs:
				return fs;
		}
	}
}

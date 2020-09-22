package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.IJobExecutor;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import asys.native.IWritable;
import asys.native.IReadable;
import asys.native.filesystem.FileLock;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import java.nio.file.Files;
import java.nio.channels.FileChannel;
import java.nio.ByteBuffer;
import java.nio.channels.FileLock as JFileLock;
import java.nio.file.FileSystemException;
import java.lang.Throwable;

@:coreApi
class File {
	public final path:FilePath;

	final channel:FileChannel;
	final jobs:IJobExecutor;
	var fs:Null<IFileSystem>;
	var deleteOnClose:Bool;
	var interProcessLock:Null<JFileLock>;

	@:allow(asys.native.filesystem)
	function new(path:FilePath, channel:FileChannel, jobs:IJobExecutor, deleteOnClose:Bool = false) {
		this.path = path;
		this.channel = channel;
		this.jobs = jobs;
		this.deleteOnClose = deleteOnClose;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		jobs.addJob(
			() -> {
				try {
					var realLength = length > buffer.length - offset ? buffer.length - offset : length;
					var jBuffer = ByteBuffer.wrap(buffer.getData(), offset, realLength);
					channel.write(jBuffer, position);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		jobs.addJob(
			() -> {
				try {
					var realLength = length > buffer.length - offset ? buffer.length - offset : length;
					var jBuffer = ByteBuffer.wrap(buffer.getData(), offset, realLength);
					var cnt = channel.read(jBuffer, position);
					cnt < 0 ? 0 : cnt;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function flush(callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					channel.force(false);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function info(callback:Callback<FileInfo>):Void {
		getFs().info(path, callback);
	}

	public function setPermissions(mode:FilePermissions, callback:Callback<NoData>):Void {
		getFs().setPermissions(path, mode, callback);
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		getFs().setOwner(path, user, group, callback);
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var current = channel.size();
					if(current > newSize) {
						channel.truncate(newSize);
					} else if(current < newSize) {
						var buffer = ByteBuffer.allocate(Int64.toInt(newSize - current));
						channel.write(buffer, current);
					}
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		getFs().setTimes(path, accessTime, modificationTime, callback);
	}

	public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					interProcessLock = switch [mode, wait] {
						case [Exclusive, true]: channel.lock();
						case [Shared, true]: channel.lock(0, java.lang.Long.MAX_VALUE, true);
						case [Exclusive, false]: channel.tryLock();
						case [Shared, false]: channel.tryLock(0, java.lang.Long.MAX_VALUE, true);
						case [Unlock, _]:
							switch interProcessLock {
								case null: null;
								case l:
									l.release();
									null;
							}
					}
					switch mode {
						case Unlock: interProcessLock == null;
						case _: interProcessLock != null;
					}
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					channel.close();
					if(deleteOnClose) {
						deleteOnClose = false;
						try Files.delete(path) catch(_) {}
					}
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	function getFs():IFileSystem {
		switch fs {
			case null:
				var fs = FileSystem.create(jobs);
				this.fs = fs;
				return fs;
			case fs:
				return fs;
		}
	}
}
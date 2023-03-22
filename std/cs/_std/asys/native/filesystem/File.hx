package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import cs.system.Exception as CsException;
import cs.system.io.File as CsFile;
import cs.system.io.FileInfo as CsFileInfo;
import cs.system.io.FileStream;
import cs.system.io.SeekOrigin;
import cs.system.io.FileAttributes;
import cs.system.DateTime;
import cs.system.DateTimeKind;
import cs.system.DateTimeOffset;
import asys.native.filesystem.FileSystem.pool;
import asys.native.filesystem.FileSystem.rethrow;
import asys.native.filesystem.FileSystem.unixEpoch;

@:coreApi
class File {
	public final path:FilePath;
	final stream:FileStream;
	final forAppend:Bool;

	@:allow(asys.native.filesystem)
	function new(stream:FileStream, path:FilePath, forAppend:Bool):Void {
		this.stream = stream;
		this.path = path;
		this.forAppend = forAppend;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		pool.runFor(
			() -> {
				try {
					if(!forAppend)
						stream.Seek(position, SeekOrigin.Begin);
					if(buffer.length < offset + length)
						length = buffer.length - offset;
					if(offset == buffer.length && length >= 0) {
						0;
					} else {
						stream.Write(buffer.getData(), offset, length);
						length;
					}
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		pool.runFor(
			() -> {
				try {
					if(buffer.length < offset + length)
						length = buffer.length - offset;
					stream.Seek(position, SeekOrigin.Begin);
					var bytesRead = stream.Read(buffer.getData(), offset, length);
					bytesRead;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function flush(callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					stream.Flush();
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function info(callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					var fi = new CsFileInfo(path);
					({
						gid: 0,
						uid: 0,
						atime: Std.int(fi.LastAccessTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						mtime: Std.int(fi.LastWriteTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						ctime: Std.int(fi.CreationTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						size: cast(fi.Length, Int),
						dev: 0,
						ino: 0,
						nlink: 0,
						rdev: 0,
						mode: @:privateAccess FileMode.S_IFREG,
						blksize: 0,
						blocks: 0
					}:FileInfo);
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var attr = (cast CsFile.GetAttributes(path):Int);
					var ro = (cast FileAttributes.ReadOnly:Int);
					if(attr & 128 == 0) // u+w
						CsFile.SetAttributes(path, cast (attr | ro))
					else
						CsFile.SetAttributes(path, cast (attr & ~ro));
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					stream.SetLength(newSize);
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var epoch = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
					CsFile.SetLastAccessTimeUtc(path, epoch.AddSeconds(accessTime));
					CsFile.SetLastWriteTimeUtc(path, epoch.AddSeconds(modificationTime));
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	/**
		TODO: this requires a separate work for design and implementation
		to find a solid cross-platform solution.

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
	// public function lock(mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>):Void {
	// 	throw new NotImplementedException();
	// }

	public function close(callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					stream.Close();
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}
}
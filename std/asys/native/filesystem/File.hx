package asys.native.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import asys.native.IWritable;
import asys.native.IReadable;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

@:coreApi
class File {
	/** The path of this file as it was at the moment of opening the file **/
	public final path:FilePath;

	function new():Void {
		path = 'stub';
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
		throw new NotImplementedException();
	}

	/**
		Read up to `length` bytes from the file `position` and write them into
		`buffer` starting at `offset` position in `buffer`, then invoke `callback`
		with the amount of bytes read.

		If `position` is greater or equal to the file size at the moment of reading
		then `0` is passed to the `callback` and `buffer` is unaffected.

		If `position` is negative or `offset` is outside of `buffer` bounds, an
		error is passed to the `callback`.
	**/
	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		throw new NotImplementedException();
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
	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
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

	/**
		Close the file.

		Does not fail if the file is already closed.
	**/
	public function close(callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}
}
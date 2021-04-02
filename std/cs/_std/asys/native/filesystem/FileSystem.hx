package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import cs.system.Exception as CsException;
import sys.thread.ElasticThreadPool;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import cs.system.io.File as CsFile;
import cs.system.io.FileNotFoundException;
import cs.system.io.DirectoryNotFoundException;
import cs.system.security.SecurityException;

@:coreApi
class FileSystem {
	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(2 * cs.system.Environment.ProcessorCount);

	/**
		Open file for reading and/or writing.

		Depending on `flag` value `callback` will be invoked with the appropriate
		object type to read and/or write the file:
		- `asys.native.filesystem.File` for reading and writing;
		- `asys.native.filesystem.FileRead` for reading only;
		- `asys.native.filesystem.FileWrite` for writing only;
		- `asys.native.filesystem.FileAppend` for writing to the end of file only;

		@see asys.native.filesystem.FileOpenFlag for more details.
	**/
	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		throw new NotImplementedException();
	}

	/**
		Create and open a unique temporary file for writing and reading.

		The file will be automatically deleted when it is closed.

		Depending on a target platform the file may be automatically deleted upon
		application shutdown, but in general deletion is not guaranteed if the `close`
		method is not called.
	**/
	static public function tempFile(callback:Callback<File>):Void {
		throw new NotImplementedException();
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		pool.runFor(
			() -> {
				try {
					Bytes.ofData(CsFile.ReadAllBytes(path));
				} catch(e:FileNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:DirectoryNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:SecurityException) {
					throw new FsException(AccessDenied, path);
				} catch(e:CsException) {
					throw new FsException(CustomError(e.Message), path);
				}
			},
			callback
		);
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		pool.runFor(
			() -> {
				try {
					CsFile.ReadAllText(path);
				} catch(e:FileNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:DirectoryNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:SecurityException) {
					throw new FsException(AccessDenied, path);
				} catch(e:CsException) {
					throw new FsException(CustomError(e.Message), path);
				}
			},
			callback
		);
	}

	/**
		Write `data` into a file specified by `path`

		`flag` controls the behavior.
		By default the file truncated if it exists and created if it does not exist.

		@see asys.native.filesystem.FileOpenFlag for more details.
	**/
	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					CsFile.WriteAllText(path, text);
					NoData;
				} catch(e:FileNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:DirectoryNotFoundException) {
					throw new FsException(FileNotFound, path);
				} catch(e:SecurityException) {
					throw new FsException(AccessDenied, path);
				} catch(e:CsException) {
					throw new FsException(CustomError(e.Message), path);
				}
			},
			callback
		);
	}

	/**
		Open directory for listing.

		`maxBatchSize` sets maximum amount of entries returned by a call to `directory.next`.

		In general bigger `maxBatchSize` allows to iterate faster, but requires more
		memory per call to `directory.next`.

		@see asys.native.filesystem.Directory.next
	**/
	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

	/**
		List directory contents.
		Does not add `.` and `..` to the result.
		Entries are provided as paths relative to the directory.
	**/
	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		throw new NotImplementedException();
	}

	/**
		Create a directory.

		Default `permissions` equals to octal `0777`, which means read+write+execution
		permissions for everyone.

		If `recursive` is `true`: create missing directories tree all the way down to `path`.
		If `recursive` is `false`: fail if any parent directory of `path` does not exist.
	**/
	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Create a directory with auto-generated unique name.

		`prefix` (if provided) is used as the beginning of a generated name.
		The created directory path is passed to the `callback`.

		Default `permissions` equals to octal `0777`, which means read+write+execution
		permissions for everyone.

		If `recursive` is `true`: create missing directories tree all the way down to the generated path.
		If `recursive` is `false`: fail if any parent directory of the generated path does not exist.
	**/
	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
	}

	/**
		Move and/or rename the file or directory from `oldPath` to `newPath`.

		If `newPath` already exists and `overwrite` is `true` (which is the default)
		the destination is overwritten. However, operation fails if `newPath` is
		a non-empty directory.

		If `overwrite` is `false` the operation is not guaranteed to be atomic.
		That means if a third-party process creates `newPath` right in between the
		check for existance and the actual move operation then the data created
		by that third-party process may be overwritten.
	**/
	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Remove a file or symbolic link.
	**/
	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Remove an empty directory.
	**/
	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Get file or directory information at the given path.
		If `path` is a symbolic link then the link is followed.

		@see `asys.native.filesystem.FileSystem.linkInfo` to get information of the
		link itself.
	**/
	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	/**
		Check user's access for a path.

		For example to check if a file is readable and writable:
		```haxe
		import asys.native.filesystem.FileAccessMode;
		FileSystem.check(path, Readable | Writable, (error, result) -> trace(result));
		```
	**/
	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	/**
		Check if the path is a directory.
		If `path` is a symbolic links then it will be resolved and checked.
		Returns `false` if `path` does not exist.
	**/
	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	/**
		Check if the path is a regular file.
		If `path` is a symbolic links then it will be resolved and checked.
		Returns `false` if `path` does not exist.
	**/
	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	/**
		Set path permissions.

		If `path` is a symbolic link it is dereferenced.
	**/
	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Set path owner and group.

		If `path` is a symbolic link it is dereferenced.
	**/
	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Set symbolic link owner and group.
	**/
	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Create a link to `target` at `path`.

		If `type` is `SymLink` the `target` is expected to be an absolute path or
		a path relative to `path`, however the existance of `target` is not checked
		and the link is created even if `target` does not exist.

		If `type` is `HardLink` the `target` is expected to be an existing path either
		absolute or relative to the current working directory.
	**/
	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Check if the path is a symbolic link.
		Returns `false` if `path` does not exist.
	**/
	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	/**
		Get the value of a symbolic link.
	**/
	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
	}

	/**
		Get information at the given path without following symbolic links.
	**/
	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	/**
		Copy a file from `source` path to `destination` path.
	**/
	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Shrink or expand a file specified by `path` to `newSize` bytes.

		If the file does not exist, it is created.

		If the file is larger than `newSize`, the extra data is lost.
		If the file is shorter, zero bytes are used to fill the added length.
	**/
	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Change access and modification times of an existing file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asys.native.filesystem.FileInfo.FileStat`
	**/
	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				var result = try {
					//C# does not have API to resolve symlinks
					CsFile.Exists(path) ? FilePath.ofString(path.absolute()) : null;
				} catch(e:CsException) {
					throw new FsException(CustomError(e.Message), path);
				}
				if(result == null)
					throw new FsException(FileNotFound, path);
				result;
			},
			callback
		);
	}
}
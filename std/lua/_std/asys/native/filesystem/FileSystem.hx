package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.Exception;
import haxe.exceptions.NotImplementedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import lua.Lib;
import lua.Table;
import lua.lib.luv.fs.FileSystem as Fs;
import lua.lib.luv.fs.FileSystem.NameType;

@:coreApi
class FileSystem {
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

		The file will be automatically deleted when it is closed or the program
		terminates.

		Depending on a target platform the file deletion may not be guaranteed if
		application crashes.

		TODO: Can Haxe guarantee automatic file deletion for all targets?
	**/
	static public function tempFile(callback:Callback<File>):Void {
		throw new NotImplementedException();
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		readFile(path, s -> Bytes.ofString(s, RawNative), callback);
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		readFile(path, s -> s, callback);
	}

	static function readFile<T>(path:FilePath, fn:(String)->T, callback:Callback<T>):Void {
		Fs.open(path, Fs.constants.O_RDONLY, 0, xpcall((e,fd) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else
				Fs.fstat(fd, xpcall(function(e,stat) {
					if(e != null)
						Fs.close(fd, xpcall((_,_) -> callback.fail(new FsException(CustomError(e), path))))
					else
						Fs.read(fd, stat.size, 0, xpcall(function(e,data) {
							if(e != null)
								Fs.close(fd, xpcall((_,_) -> callback.fail(new FsException(CustomError(e), path))))
							else
								Fs.close(fd, xpcall((_,_) -> callback.success(fn(data))));
						}));
				}));
		}));
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeFile(path, data.getString(0, data.length, RawNative), flag, callback);
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeFile(path, text, flag, callback);
	}

	static function writeFile<T>(path:FilePath, data:String, flag:FileOpenFlag<Dynamic>, callback:Callback<NoData>):Void {
		Fs.open(path, luvOpenFlags(flag), FilePermissions.octal(0, 6, 4, 4), xpcall((e,fd) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else
				Fs.write(fd, data, 0, xpcall(function(e,bytesWritten) {
					if(e != null)
						Fs.close(fd, xpcall((_,_) -> callback.fail(new FsException(CustomError(e), path))))
					else
						Fs.close(fd, xpcall((e,_) -> {
							if(e != null)
								callback.fail(new FsException(CustomError(e), path))
							else
								callback.success(NoData);
						}));
				}));
		}));
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		Fs.opendir(path, xpcall((e,dir) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else {
				var result = [];
				function collect(e, entries:Table<Int,NameType>) {
					if(e != null) {
						Fs.closedir(dir, xpcall((_,_) -> callback.fail(new FsException(CustomError(e), path))));
					} else if(entries == null) {
						Fs.closedir(dir, xpcall((_,_) -> callback.success(result)));
					} else {
						//TODO: do this without intermediate array
						var entries = Table.toArray(entries);
						for(entry in entries)
							result.push(entry.name);
						Fs.readdir(dir, xpcall(collect));
					}
				}
				Fs.readdir(dir, xpcall(collect));
			}
		}));
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

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

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

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		Fs.readlink(path, xpcall((e, r:String)-> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else
				callback.success(r);
		}));
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	/**
		Copy a file from `source` path to `destination` path.
	**/
	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		Fs.open(path, Fs.constants.O_CREAT | Fs.constants.O_WRONLY, FilePermissions.octal(0, 6, 4, 4), xpcall((e,fd) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else
				Fs.ftruncate(fd, newSize, xpcall((e,r) -> {
					if(e != null)
						callback.fail(new FsException(CustomError(e), path))
					else
						Fs.close(fd, xpcall((e,_) -> {
							if(e != null)
								callback.fail(new FsException(CustomError(e), path))
							else
								callback.success(NoData);
						}));
				}));
		}));
	}

	/**
		Change access and modification times of an existing file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asys.native.filesystem.FileInfo.FileStat`
	**/
	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		Fs.realpath(path, xpcall((e, r:String) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path))
			else
				callback.success(r);
		}));
	}

	/**
		Adds exceptions handling to callbacks.
		Otherwise lua just prints `Uncaught Error: (null)` on unhandled exceptions.
	**/
	static inline function xpcall<T>(cb:(e:String, r:T)->Void):(e:String, r:T)->Void {
		return (e, r) -> lua.Lua.xpcall(() -> cb(e, r), untyped __lua__('_hx_error'));
	}

	static function luvOpenFlags(flag:FileOpenFlag<Dynamic>):Int {
		return switch flag {
			case Append: Fs.constants.O_WRONLY | Fs.constants.O_APPEND | Fs.constants.O_CREAT;
			case Read: Fs.constants.O_RDONLY;
			case ReadWrite: Fs.constants.O_RDWR;
			case Write: Fs.constants.O_WRONLY | Fs.constants.O_CREAT | Fs.constants.O_TRUNC;
			case WriteX: Fs.constants.O_WRONLY | Fs.constants.O_CREAT | Fs.constants.O_EXCL;
			case WriteRead: Fs.constants.O_RDWR | Fs.constants.O_CREAT | Fs.constants.O_TRUNC;
			case WriteReadX: Fs.constants.O_RDWR | Fs.constants.O_CREAT | Fs.constants.O_EXCL;
			case Overwrite: Fs.constants.O_WRONLY | Fs.constants.O_CREAT;
			case OverwriteRead: Fs.constants.O_RDWR | Fs.constants.O_CREAT;
		}
	}
}
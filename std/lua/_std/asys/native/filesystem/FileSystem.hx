package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.Exception;
import haxe.exceptions.NotImplementedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import lua.Lib;
import lua.lib.luv.fs.FileSystem as LFileSystem;

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
		throw new NotImplementedException();
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		throw new NotImplementedException();
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

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

	/**
		Get the value of a symbolic link.
	**/
	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
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
		LFileSystem.realpath(path, (e,r) -> {
			if(e != null)
				callback.fail(new FsException(CustomError(e), path));
			callback.success(@:privateAccess new FilePath(r));
		});
	}
}
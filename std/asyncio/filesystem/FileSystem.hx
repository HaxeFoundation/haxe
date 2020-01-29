package asyncio.filesystem;

import haxe.io.Bytes;
import asyncio.system.SystemUser;
import asyncio.system.SystemGroup;
import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;

/**
	File system operations.
**/
class FileSystem {
	/**
		Open file for reading and/or writing.

		Depending on `flags` value `callback` will be invoked with the appropriate
		object type to read and/or write the file:
		- `asyncio.filesystem.File` for reading and writing;
		- `asyncio.filesystem.FileRead` for reading only;
		- `asyncio.filesystem.FileWrite` for writing only;
		- `asyncio.filesystem.FileAppend` for writing to the end of file only;

		@see asyncio.filesystem.FileOpenFlag for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function openFile<T>(path:FilePath, flags:FileOpenFlag<T>, mode:FileAccessMode = 438, callback:Callback<Null<T>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Create and open a unique temporary file for writing and reading.

		The file will be automatically deleted when it is closed or the program terminates.

		TODO: Can Haxe guarantee automatic file deletion for all targets?
	**/
	static public function tempFile(path:FilePath, callback:Callback<Null<File>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Read the contents of a file specified by `path`.
	**/
	static public function readFile(path:FilePath, callback:Callback<Null<Bytes>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Read the contents of a file specified by `path` as a `String`.
	**/
	static public function readText(path:FilePath, callback:Callback<Null<String>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Write `data` into a file specified by `path`

		`flags` controls the behavior.
		By default the file truncated if it exists and created if it does not exist.

		@see asyncio.filesystem.FileOpenFlag for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function writeFile(path:FilePath, data:Bytes, flags:FileOpenFlag<Dynamic> = Write, mode:FileAccessMode = 438, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Write `text` into a file specified by `path`

		`flags` controls the behavior.
		By default the file is truncated if it exists and is created if it does not exist.

		@see asyncio.filesystem.FileOpenFlag for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function writeText(path:FilePath, text:String, flags:FileOpenFlag<Dynamic> = Write, mode:FileAccessMode = 438, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory(path:FilePath, callback:Callback<Null<Directory>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Create a directory.

		Default `mode` equals to octal `0777`, which means read+write+execution
		permissions for everyone.

		If `recursive` is `true`: create missing directories tree all the way down to `path`.
		If `recursive` is `false`: fail if any parent directory of `path` does not exist.
	**/
	static public function createDirectory(path:FilePath, mode:FileAccessMode = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Create a unique temporary directory.

		For a directory name `prefix` gets appended with random characters.
		The created directory path is passed to the `callback`.

		Created directory will _not_ be deleted automatically.
	**/
	static public function createTempDirectory(prefix:FilePath, callback:Callback<Null<FilePath>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Renames the file or directory located at `oldPath` to `newPath`.

		If `newPath` already exists and `overwrite` is `true` (which is the default)
		the destination is overwritten.
	**/
	static public function rename(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Remove a file or symbolic link.
	**/
	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Remove an empty directory.
	**/
	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Recursively remove everything at the given `path`.

		Removes files, symbolic links and recursively removes directories and their contents.
	**/
	static public function deleteRecursively(path:FilePath, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Get file or directory information at the given path.
	**/
	static public function info(path:FilePath, callback:Callback<Null<FileInfo>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Check user's access for a path.

		Example:
		```haxe
		import asyncio.filesystem.FileAccessMode;
		//check path existence
		FileSystem.check(path, Exists, (error, result) -> trace(result));
		//check if file is executable
		FileSystem.check(path, Executable, (error, result) -> trace(result));
		//check if file is readable and writable
		FileSystem.check(path, Readable | Writable, (error, result) -> trace(result));
		```
	**/
	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Set path permissions.

		If `recursive` is `true` and `path` points to a directory: apply `mode`
		recursively to the directory contents as well.
	**/
	static public function setPermissions(path:FilePath, mode:FileAccessMode, recursive:Bool = false, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Set path owner.

		If `recursive` is `true` and `path` points to a directory: apply recursively
		to the directory contents as well.
	**/
	static public function setOwner(path:FilePath, user:SystemUser, ?group:SystemGroup, recursive:Bool = false, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Create a link to `target` at `path`.

		If `path` is omitted a link to `target` will be created in the current
		directory with the same name as the last component of `target` path.
		For example `FileSystem.link('/path/to/file.ext', callback)` will create
		a link named `file.ext` in the current directory.
	**/
	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Get the value of a symbolic link.
	**/
	static public function readLink(path:FilePath, callback:Callback<Null<FilePath>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Copy a file from `source` path to `destination` path.
	**/
	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Copy all the contents of `source` path to `destination` path.
		If `source` is a directory, it will be copied recursively.
	**/
	static public function copy(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Shrink or expand a file specified by `path` to `newSize` bytes.

		If the file does not exist, it is created.

		If the file is larger than `newSize`, the extra data is lost.
		If the file is shorter, zero bytes are used to fill the added length.
	**/
	static public function resizeFile(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Change access and modification times of a file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asyncio.filesystem.FileInfo.FileStat`
	**/
	static public function setFileTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
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
	static public function lock(file:File, mode:FileLock = Exclusive, wait:Bool = true, callback:Callback<Bool>) {
		callback.fail(new NotImplemented());
	}
}
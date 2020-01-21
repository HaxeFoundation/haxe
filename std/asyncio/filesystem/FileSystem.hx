package asyncio.filesystem;

import haxe.io.Bytes;
import asyncio.system.SystemUser;
import asyncio.system.SystemGroup;
import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;

/**
	File system operations.

	TODO:
	Decide on naming convention:
	- Follow unix names: does not match `sys.FileSystem` names and may add unnecessary
		difficulties for new users, which are not familiar with unix.
	- Follow `sys.FileSystem`, which does not use unix names (most of the time),
		but then something like `info` instead of `stat` is kind of weird.
**/
class FileSystem {
	/**
		Open file for reading and/or writing.

		Depending on `flags` value `callback` will be invoked with the appropriate
		object type to read and/or write the file:
		- `aio.filesystem.File` for reading and writing;
		- `aio.filesystem.FileRead` for reading only;
		- `aio.filesystem.FileWrite` for writing only;
		- `aio.filesystem.FileAppend` for writing to the end of file only;

		@see `asyncio.filesystem.FileOpenFlag` for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function openFile<T>(path:FilePath, flags:FileOpenFlag<T>, mode:FileAccessMode = 438, callback:Callback<Null<T>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Create and open a unique temporary file for writing and reading.

		The file will be automatically deleted when it is closed or the program terminates.

		TODO: Can Haxe guarantee automatic file deletion for all targets?
	**/
	static public function tempFile(path:FilePath, callback:Callback<Null<File>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Read the contents of a file specified by `path`.
	**/
	static public function readFile(path:FilePath, callback:Callback<Null<Bytes>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Read the contents of a file specified by `path` as a `String`.
	**/
	static public function readText(path:FilePath, callback:Callback<Null<String>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Write `data` into a file specified by `path`

		`flags` controls the behavior.
		By default the file truncated if it exists and created if it does not exist.

		@see `asyncio.filesystem.FileOpenFlag` for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function writeFile(path:FilePath, data:Bytes, flags:FileOpenFlag<Dynamic> = Write, mode:FileAccessMode = 438, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Write `text` into a file specified by `path`

		`flags` controls the behavior.
		By default the file truncated if it exists and created if it does not exist.

		@see `asyncio.filesystem.FileOpenFlag` for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flags` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function writeText(path:FilePath, text:String, flags:FileOpenFlag<Dynamic> = Write, mode:FileAccessMode = 438, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory<T>(path:FilePath, callback:Callback<Null<T>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Create a directory.

		Default `mode` equals to octal `0777`, which means read+write+execution
		permissions for everyone.

		If `recursive` is `true`: create missing directories tree all the way down to `path`.
		If `recursive` is `false`: fail if any parent directory of `path` does not exist.
	**/
	static public function createDirectory(path:FilePath, mode:FileAccessMode = 438, recursive:Bool = false, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Create a unique temporary directory.

		For a directory name `prefix` gets appended with random characters.
		The created directory path is passed to the `callback`.

		Created directory will _not_ be deleted automatically.
	**/
	static public function createTempDirectory(prefix:FilePath, callback:Callback<Null<FilePath>>):Void {
		callback(new NotImplemented(), null);
	}

	/**
		Remove a file or symbolic link.
	**/
	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Remove an empty directory.
	**/
	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Recursively remove everything at the given `path`.

		Removes files, symbolic links and recursively removes directories and their contents.
	**/
	static public function deleteRecursive(path:FilePath, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Get file or directory information at the given path.
	**/
	static public function info(path:FilePath, callback:Callback<Null<FileInfo>>):Void {
		callback(new NotImplemented(), null);
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
		callback(new NotImplemented(), false);
	}

	/**
		Set path permissions.
	**/
	static public function setPermissions(path:FilePath, mode:FileAccessMode, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Set path owner.
	**/
	static public function setOwner(path:FilePath, user:SystemUser, ?group:SystemGroup, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Create a link to `target` at `path`.

		If `path` is omitted a link to `target` will be created in the current
		directory with the same name as the last component of `target` path.
		For example `FileSystem.link('/path/to/file.ext', callback)` will create
		a link named `file.ext` in the current directory.
	**/
	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Copy a file from `source` path to `destination` path.
	**/
	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Copy all the contents of `source` path to `destination` path.
	**/
	static public function copy(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Shrink or expand a file specified by `path` to `newSize` bytes.

		If the file does not exist, it is created.

		If the file is larger than `newSize`, the extra data is lost.
		If the file is shorter, zero bytes are used to fill the added length.
	**/
	static public function resizeFile(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}
}
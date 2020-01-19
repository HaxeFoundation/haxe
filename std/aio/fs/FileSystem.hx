package aio.fs;

import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;

class FileSystem {
	/**
		Open file for reading and/or writing.

		Depending on `flags` value `callback` will be invoked with the appropriate
		object type to read and/or write the file:
		- `aio.fs.File` for reading and writing;
		- `aio.fs.FileRead` for reading only;
		- `aio.fs.FileWrite` for writing only;
		- `aio.fs.FileAppend` for writing to the end of file only;

		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function openFile<T>(path:FilePath, flags:FileOpenFlag<T>, mode:FileAccessMode = 438, callback:Callback<Null<T>>):Void {
		callback(new NotImplemented(), null);
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


}
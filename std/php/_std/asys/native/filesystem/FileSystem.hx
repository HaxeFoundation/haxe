package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import php.Global.*;
import php.Resource;

/**
	File system operations.
**/
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

		`mode` is used to set permissions for a created file in case of appropriate
		`flag` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		throw new NotImplementedException();
	}

	/**
		Create and open a unique temporary file for writing and reading.

		The file will be automatically deleted when it is closed or the program terminates.

		TODO: Can Haxe guarantee automatic file deletion for all targets?
	**/
	static public function tempFile(path:FilePath, callback:Callback<File>):Void {
		throw new NotImplementedException();
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				file_get_contents(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case r: callback.success(Bytes.ofString(r));
			}
		});
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				file_get_contents(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case r: callback.success(r);
			}
		});
	}

	/**
		Write `data` into a file specified by `path`

		`flag` controls the behavior.
		By default the file truncated if it exists and created if it does not exist.

		@see asys.native.filesystem.FileOpenFlag for more details.

		`mode` is used to set permissions for a created file in case of appropriate
		`flag` are chosen.
		Default `mode` equals to octal `0666`, which means read+write permissions
		for everyone.
	**/
	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var f = fopenHx(cast path, flag);
				fwrite(f, data.getData().toString());
				fclose(f);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var f = fopenHx(cast path, flag);
				fwrite(f, text);
				fclose(f);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

	/**
		Create a directory.

		Default `mode` equals to octal `0777`, which means read+write+execution
		permissions for everyone.

		If `recursive` is `true`: create missing directories tree all the way down to `path`.
		If `recursive` is `false`: fail if any parent directory of `path` does not exist.
	**/
	static public function createDirectory(path:FilePath, mode:FileAccessMode = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Create a unique temporary directory.

		For a directory name `prefix` gets appended with random characters.
		The created directory path is passed to the `callback`.

		Created directory will _not_ be deleted automatically.

		TODO: is it really "temporary"? Probably "unique" would be a better name.
	**/
	static public function createTempDirectory(prefix:FilePath, callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
	}

	/**
		Renames the file or directory located at `oldPath` to `newPath`.

		If `newPath` already exists and `overwrite` is `true` (which is the default)
		the destination is overwritten.
	**/
	static public function rename(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
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
		Remove everything at the given `path`.

		Removes files, symbolic links and recursively removes directories and their contents.
	**/
	static public function delete(path:FilePath, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Get file or directory information at the given path.
	**/
	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	/**
		Check user's access for a path.

		Example:
		```haxe
		import asys.native.filesystem.FileAccessMode;
		//check path existence
		FileSystem.check(path, Exists, (error, result) -> trace(result));
		//check if file is executable
		FileSystem.check(path, Executable, (error, result) -> trace(result));
		//check if file is readable and writable
		FileSystem.check(path, Readable | Writable, (error, result) -> trace(result));
		```
	**/
	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		throw new NotImplementedException();
	}

	/**
		Set path permissions.

		If `recursive` is `true` and `path` points to a directory: apply `mode`
		recursively to the directory contents as well.
	**/
	static public function setPermissions(path:FilePath, mode:FileAccessMode, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Set path owner and group.

		If `recursive` is `true` and `path` points to a directory: apply recursively
		to the directory contents as well.
	**/
	static public function setOwner(path:FilePath, user:SystemUser, ?group:SystemGroup, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Set path owning group.

		If `recursive` is `true` and `path` points to a directory: apply recursively
		to the directory contents as well.
	**/
	static public function setGroup(path:FilePath, group:SystemGroup, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		if(path == null) {
			path = basename(cast target);
		}
		EntryPoint.runInMainThread(() -> {
			var success = try {
				switch type {
					case SymLink: symlink(cast target, cast path);
					case HardLink: php.Global.link(cast target, cast path);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to create a link'), path));
			}
		});
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				is_link(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				readlink(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a link'), path));
				case r: callback.success(r);
			}
		});
	}

	/**
		Copy a file from `source` path to `destination` path.
	**/
	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	/**
		Copy all the contents of `source` path to `destination` path.
		If `source` is a directory, it will be copied recursively.
	**/
	static public function copy(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
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
		Change access and modification times of a file.

		TODO: Decide on type for `accessTime` and `modificationTime` - see TODO in `asys.native.filesystem.FileInfo.FileStat`
	**/
	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static function fopenHx(file:String, flag:FileOpenFlag<Dynamic>):Resource {
		var f = switch flag {
			case Append: fopen(file, 'a');
			case AppendRead: fopen(file, 'a+');
			case Read: fopen(file, 'r');
			case ReadWrite: fopen(file, 'r+');
			case Write: fopen(file, 'w');
			case WriteX: fopen(file, 'x');
			case WriteRead: fopen(file, 'w+');
			case WriteReadX: fopen(file, 'x+');
			case Overwrite: fopen(file, 'c');
			case OverwriteRead: fopen(file, 'c+');
		}
		if(f == false)
			throw new FsException(CustomError('Cannot open file'), file);
		return f;
	}
}
package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import php.Global.*;
import php.Syntax;
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

	static public function tempFile(callback:Callback<File>):Void {
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

	static public function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				mkdir(cast path, permissions, recursive);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to create a directory'), path));
			}
		});
	}

	/**
		Create a unique temporary directory.

		For a directory name `prefix` gets appended with random characters.
		The created directory path is passed to the `callback`.

		Created directory will _not_ be deleted automatically.

		TODO: is it really "temporary"? Probably "unique" would be a better name.
	**/
	static public function tempDirectory(prefix:FilePath, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			if(!overwrite && file_exists(cast newPath)) {
				callback.fail(new FsException(FileExists, newPath));
				return;
			}
			var success = try {
				moveRecursive(cast oldPath, cast newPath);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), oldPath));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to move file or directory'), oldPath));
			}
		});
	}

	/**
	 * This is required to avoid "Directory not empty" warning from `rename` function
	 */
	static function moveRecursive(oldPath:String, newPath:String):Bool {
		if(is_dir(newPath) && is_dir(oldPath)) {
			var dir = opendir(oldPath);
			var success = true;
			if(dir == false)
				throw new FsException(CustomError('Failed to read directory'), oldPath);
			try {
				while(true) {
					switch readdir(dir) {
						case '.' | '..':
						case false:
							break;
						case entry:
							success = moveRecursive('$oldPath/$entry', '$newPath/$entry') && success;
					}
				}
			} catch(e:php.Exception) {
				try closedir(dir) catch(_) {}
				throw e;
			}
			return success;
		} else {
			return rename(oldPath, newPath);
		}
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				unlink(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to delete a file'), path));
			}
		});
	}

	/**
		Remove an empty directory.
	**/
	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				rmdir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to delete a file'), path));
			}
		});
	}

	/**
		Get file or directory information at the given path.
	**/
	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new NotImplementedException();
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
					(!mode.has(Exists) || file_exists(cast path))
					&& (!mode.has(Readable) || is_readable(cast path))
					&& (!mode.has(Writable) || is_writable(cast path))
					&& (!mode.has(Executable) || is_executable(cast path));
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
					is_dir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
					is_file(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	/**
		Set path permissions.

		If `recursive` is `true` and `path` points to a directory: apply `mode`
		recursively to the directory contents as well.
	**/
	static public function setPermissions(path:FilePath, permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
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
		var path:FilePath = path == null ? basename(cast target) : path;
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

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			if(!overwrite && file_exists(cast destination)) {
				callback.fail(new FsException(FileExists, destination));
				return;
			}
			var success = try {
				copy(cast source, cast destination);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), source));
				return;
			}
			if (success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to copy a file'), source));
			}
		});
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
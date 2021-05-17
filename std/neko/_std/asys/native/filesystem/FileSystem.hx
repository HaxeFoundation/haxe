package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.Exception;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import sys.thread.ElasticThreadPool;
import neko.Lib;

using neko.NativeString;

private typedef FileStat = {
	var gid:Int;
	var uid:Int;
	var atime:Float;
	var mtime:Float;
	var ctime:Float;
	var size:Int;
	var dev:Int;
	var ino:Int;
	var nlink:Int;
	var rdev:Int;
	var mode:Int;
}

private enum FileHandle {}

@:coreApi
class FileSystem {
	static final sys_exists:(NativeString)->Bool = Lib.load("std", "sys_exists", 1);
	static final file_delete = Lib.load("std", "file_delete", 1);
	static final sys_rename = Lib.load("std", "sys_rename", 2);
	static final sys_stat:(NativeString)->FileStat = Lib.load("std", "sys_stat", 1);
	static final sys_file_type:(NativeString)->NativeString = Lib.load("std", "sys_file_type", 1);
	static final sys_create_dir = Lib.load("std", "sys_create_dir", 2);
	static final sys_remove_dir = Lib.load("std", "sys_remove_dir", 1);
	static final sys_read_dir:(NativeString)->Array<Any> = Lib.load("std", "sys_read_dir", 1);
	static final file_full_path:(NativeString)->NativeString = Lib.load("std", "file_full_path", 1);
	static final file_contents:(NativeString)->NativeString = neko.Lib.load("std", "file_contents", 1);
	static final file_open:(path:NativeString, mode:NativeString)->FileHandle = neko.Lib.load("std", "file_open", 2);
	static final file_close:(FileHandle)->Void = neko.Lib.load("std", "file_close", 1);
	static final file_seek = neko.Lib.load("std", "file_seek", 3);
	static final file_tell = neko.Lib.load("std", "file_tell", 1);
	static final file_flush = neko.Lib.load("std", "file_flush", 1);
	static final file_write:(file:FileHandle, data:NativeString, pos:Int, length:Int)->Int = neko.Lib.load("std", "file_write", 4);
	static final file_write_char = neko.Lib.load("std", "file_write_char", 2);

	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(4);

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
		pool.runFor(
			() -> {
				try {
					Lib.bytesReference(file_contents(path).toString());
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		pool.runFor(
			() -> {
				try {
					file_contents(path).toString();
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> inline writeToFile(path, data.getData(), flag),
			callback
		);
	}

	static function writeToFile(path:FilePath, data:NativeString, flag:FileOpenFlag<Dynamic>):NoData {
		var f = null;
		try {
			// f = file_open(path, fopenHx(path, flag));
			f = file_open(path, NativeString.ofString('wb'));
			var length = data.length();
			var pos = 0;
			while (length > 0) {
				var bytesWritten = file_write(f, data, pos, length);
				if (bytesWritten == 0)
					throw new Exception('Blocked');
				pos += bytesWritten;
				length -= bytesWritten;
			}
			return NoData;
		} catch(e) {
			if(f != null)
				try file_close(f) catch(_) {}
			throw new FsException(CustomError(e.toString()), path);
		}
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> inline writeToFile(path, NativeString.ofString(text), flag),
			callback
		);
	}

	/**
		Open directory for listing.
	**/
	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		throw new NotImplementedException();
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				try {
					var list = sys_read_dir(path);
					var result = [];
					while (list != null) {
						result.push(FilePath.ofString((list[0]:NativeString).toString()));
						list = list[1];
					}
					result;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
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
		pool.runFor(
			() -> {
				try {
					var data = sys_stat(path);
					data.atime = Std.int(data.atime / 1000);
					data.ctime = Std.int(data.ctime / 1000);
					data.mtime = Std.int(data.mtime / 1000);
					cast data;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
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
		pool.runFor(
			() -> {
				try {
					sys_file_type(path).toString() == "dir";
				} catch(e) {
					if(!sys_exists(path))
						false
					else
						throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					sys_file_type(path).toString() == "file";
				} catch(e) {
					if(!sys_exists(path))
						false
					else
						throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
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

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		throw NotSupportedException.field();
		// pool.runFor(
		// 	() -> {
		// 		try {
		// 			var info:FileInfo = cast sys_stat(path);
		// 			info.mode.isLink();
		// 		} catch(e) {
		// 			if(!sys_exists(path))
		// 				false
		// 			else
		// 				throw new FsException(CustomError(e.toString()), path);
		// 		}
		// 	},
		// 	callback
		// );
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		throw NotSupportedException.field();
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		throw NotSupportedException.field();
		// pool.runFor(
		// 	() -> {
		// 		try {
		// 			var data = sys_stat(path);
		// 			data.atime = Std.int(data.atime / 1000);
		// 			data.ctime = Std.int(data.ctime / 1000);
		// 			data.mtime = Std.int(data.mtime / 1000);
		// 			cast data;
		// 		} catch(e) {
		// 			throw new FsException(CustomError(e.toString()), path);
		// 		}
		// 	},
		// 	callback
		// );
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
				try {
					FilePath.ofString(file_full_path(path).toString());
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	// static function fopenHx(file:NativeString, flag:FileOpenFlag<Dynamic>):Resource {
	// 	var flags = switch flag {
	// 		case Append: 'ab';
	// 		case Read: 'rb';
	// 		case ReadWrite: 'rb+';
	// 		case Write: 'wb';
	// 		case WriteX: ;
	// 		case WriteRead: ;
	// 		case WriteReadX: ;
	// 		case Overwrite: ;
	// 		case OverwriteRead: ;
	// 	}
	// 	return file_open(file, NativeString.ofString(flags));
	// }
}
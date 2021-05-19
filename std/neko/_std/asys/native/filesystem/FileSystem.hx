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

@:coreApi
@:allow(asys.native.filesystem)
class FileSystem {
	static final sys_exists:(NativeString)->Bool = Lib.load("std", "sys_exists", 1);
	static final file_delete:(NativeString)->Void = Lib.load("std", "file_delete", 1);
	static final sys_rename:(NativeString, NativeString)->Void = Lib.load("std", "sys_rename", 2);
	static final sys_stat:(NativeString)->FileStat = Lib.load("std", "sys_stat", 1);
	static final sys_file_type:(NativeString)->NativeString = Lib.load("std", "sys_file_type", 1);
	static final sys_create_dir:(path:NativeString,mode:Int)->Void = Lib.load("std", "sys_create_dir", 2);
	static final sys_remove_dir:(NativeString)->Void = Lib.load("std", "sys_remove_dir", 1);
	static final sys_read_dir:(NativeString)->Array<Any> = Lib.load("std", "sys_read_dir", 1);
	static final file_full_path:(NativeString)->NativeString = Lib.load("std", "file_full_path", 1);
	static final file_contents:(NativeString)->NativeString = neko.Lib.load("std", "file_contents", 1);
	static final file_open:(path:NativeString, mode:NativeString)->FileHandle = neko.Lib.load("std", "file_open", 2);
	static final file_close:(FileHandle)->Void = neko.Lib.load("std", "file_close", 1);
	static final file_seek:(f:FileHandle, pos:Int, kind:Int)->Void = neko.Lib.load("std", "file_seek", 3);
	static final file_tell:(FileHandle)->Int = neko.Lib.load("std", "file_tell", 1);
	static final file_flush:(FileHandle)->Void = neko.Lib.load("std", "file_flush", 1);
	static final file_write:(file:FileHandle, data:NativeString, pos:Int, length:Int)->Int = neko.Lib.load("std", "file_write", 4);
	static final file_write_char = neko.Lib.load("std", "file_write_char", 2);
	static final file_eof:(FileHandle)->Bool = neko.Lib.load("std", "file_eof", 1);
	static final file_read:(f:FileHandle, buf:NativeString, pos:Int, length:Int)->Int = neko.Lib.load("std", "file_read", 4);
	static final file_read_char:(FileHandle)->Int = neko.Lib.load("std", "file_read_char", 1);

	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(4);

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		pool.runFor(
			() -> {
				try {
					cast new File(fopenHx(path, flag), path, false);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function tempFile(callback:Callback<File>):Void {
		pool.runFor(
			() -> {
				try {
					var name = getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
					var dir = FilePath.ofString(Sys.getCwd());
					var path = dir.add(name);
					while(sys_exists(path)) {
						name += getRandomChar();
					}
					cast new File(fopenHx(path, WriteRead), path, true);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), '(unknown path)');
				}
			},
			callback
		);
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
			f = fopenHx(path, flag);
			var length = data.length();
			var pos = 0;
			while (length > 0) {
				var bytesWritten = file_write(f, data, pos, length);
				if (bytesWritten == 0)
					throw new Exception('Blocked');
				pos += bytesWritten;
				length -= bytesWritten;
			}
			file_close(f);
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

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		if(permissions == null) permissions = 511;
		pool.runFor(
			() -> {
				try {
					mkdir(path, permissions, recursive);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static function mkdir(path:FilePath, mode:Int, recursive:Bool):Void {
		if(recursive) {
			var parent = path.parent();
			if(!sys_exists(parent)) {
				mkdir(parent, mode, recursive);
			}
		}
		sys_create_dir(path, mode);
	}

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		if(permissions == null) permissions = 511;
		pool.runFor(
			() -> {
				try {
					prefix = (prefix == null ? '' : prefix) + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
					var path = parentDirectory.add(prefix);
					while(sys_exists(path)) {
						prefix += getRandomChar();
						path = parentDirectory.add(prefix);
					}
					mkdir(path, permissions, recursive);
					path;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), parentDirectory);
				}
			},
			callback
		);
	}

	static var __codes:Null<Array<String>>;
	static function getRandomChar():String {
		switch __codes {
			case null:
				var a = [for(c in '0'.code...'9'.code) String.fromCharCode(c)];
				for(c in 'A'.code...'Z'.code) a.push(String.fromCharCode(c));
				for(c in 'a'.code...'z'.code) a.push(String.fromCharCode(c));
				__codes = a;
				return a[Std.random(a.length)];
			case a:
				return a[Std.random(a.length)];
		}
	}

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(!overwrite && sys_exists(newPath))
						throw new FsException(FileExists, newPath);
					sys_rename(oldPath, newPath);
					NoData;
				} catch(e:FsException) {
					throw e;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), oldPath);
				}
			},
			callback
		);
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					file_delete(path);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					sys_remove_dir(path);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
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

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		switch mode {
			case Executable:
				throw new NotSupportedException('File access mode "Executable" is not supported on neko');
			case Writable:
				throw new NotSupportedException('File access mode "Writable" is not supported on neko');
			case Readable:
				throw new NotSupportedException('File access mode "Readable" is not supported on neko');
			case Exists:
				pool.runFor(
					() -> {
						try {
							sys_exists(path);
						} catch(e) {
							throw new FsException(CustomError(e.toString()), path);
						}
					},
					callback
				);
		}
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

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
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

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(!overwrite && sys_exists(destination))
						throw new FsException(FileExists, destination);
					sys.io.File.copy(source, destination);
					NoData;
				} catch(e:FsException) {
					throw e;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), source);
				}
			},
			callback
		);
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw NotSupportedException.field();
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

	static function fopenHx(path:FilePath, flag:FileOpenFlag<Dynamic>):FileHandle {
		var flags = switch flag {
			case Append: 'ab';
			case Read: 'rb';
			case ReadWrite: 'rb+';
			case Write: 'wb';
			case WriteX: 'wxb';
			case WriteRead: 'wb+';
			case WriteReadX: 'wxb+';
			case Overwrite: throw new NotSupportedException('"Overwrite" flag is not supported on neko');
			case OverwriteRead:  throw new NotSupportedException('"OverwriteRead" flag is not supported on neko');
		}
		return file_open(path, NativeString.ofString(flags));
	}
}
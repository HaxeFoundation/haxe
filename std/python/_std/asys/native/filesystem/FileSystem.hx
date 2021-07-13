package asys.native.filesystem;

import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import python.lib.Builtins;
import python.lib.Os;
import python.lib.os.Path;
import python.lib.Shutil;
import python.Tuple;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.Exception;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import sys.thread.ElasticThreadPool;

@:coreApi
@:allow(asys.native.filesystem)
class FileSystem {

	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(4);

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		throw new NotImplementedException();
		// pool.runFor(
		// 	() -> {
		// 		try {
		// 			cast new File(fopenHx(path, flag), path, false);
		// 		} catch(e) {
		// 			throw new FsException(CustomError(e.toString()), path);
		// 		}
		// 	},
		// 	callback
		// );
	}

	static public function tempFile(callback:Callback<File>):Void {
		throw new NotImplementedException();
		// pool.runFor(
		// 	() -> {
		// 		try {
		// 			var name = getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
		// 			var dir = FilePath.ofString(Sys.getCwd());
		// 			var path = dir.add(name);
		// 			while(sys_exists(path)) {
		// 				name += getRandomChar();
		// 			}
		// 			cast new File(fopenHx(path, WriteRead), path, true);
		// 		} catch(e) {
		// 			throw new FsException(CustomError(e.toString()), '(unknown path)');
		// 		}
		// 	},
		// 	callback
		// );
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		pool.runFor(
			() -> {
				try {
					var f:RawIOBase = cast Builtins.open(path, 'rb', -1);
					var size = f.read(-1);
					var data = haxe.io.Bytes.ofData(size);
					f.close();
					data;
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
					var f:TextIOBase = cast Builtins.open(path, 'r', -1, "utf-8", null, "");
					var content = f.read(-1);
					f.close();
					content;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var f:RawIOBase = cast Os.fdopen(Os.open(path, pyOpenFlags(flag, true)), 'rb+', -1);
					f.write(data.getData());
					f.close();
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var f:TextIOBase = cast Os.fdopen(Os.open(path, pyOpenFlags(flag, false)), 'r+', -1, "utf-8", null, "");
					f.write(text);
					f.close();
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
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
					cast Os.listdir(path);
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
					if(recursive)
						Os.makedirs(path, permissions)
					else
						Os.mkdir(path, permissions);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		if(permissions == null) permissions = 511;
		pool.runFor(
			() -> {
				try {
					prefix = (prefix == null ? '' : prefix) + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
					var path = parentDirectory.add(prefix);
					while(Path.exists(path)) {
						prefix += getRandomChar();
						path = parentDirectory.add(prefix);
					}
					if(recursive)
						Os.makedirs(path, permissions)
					else
						Os.mkdir(path, permissions);
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
					if(overwrite)
						Os.replace(oldPath, newPath)
					else
						Os.rename(oldPath, newPath);
					NoData;
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
					Os.remove(path);
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
					Os.rmdir(path);
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
					Os.stat(path);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					var checks = 0;
					if(mode.has(Exists)) checks = checks | Os.F_OK;
					if(mode.has(Executable)) checks = checks | Os.X_OK;
					if(mode.has(Writable)) checks = checks | Os.W_OK;
					if(mode.has(Readable)) checks = checks | Os.R_OK;
					Os.access(path, checks);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					Path.isdir(path);
				} catch(e) {
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
					Path.isfile(path);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Os.chmod(path, permissions);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(Os.name == 'posix')
						Os.chown(path, user, group);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(Os.name == 'posix')
						Os.lchown(path, user, group);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					switch type {
						case SymLink: Os.symlink(target, path);
						case HardLink: Os.link(target, path);
					}
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					Path.islink(path);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				try {
					FilePath.ofString(Os.readlink(path));
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					Os.lstat(path);
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				if(!overwrite && Path.exists(destination))
					throw new FsException(FileExists, destination);
				try {
					Shutil.copyfile(source, destination);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), source);
				}
			},
			callback
		);
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var fd = Os.open(path, Os.O_WRONLY | Os.O_CREAT);
					Os.ftruncate(fd, newSize);
					Os.close(fd);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Os.utime(path, Tuple2.make(accessTime, modificationTime));
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				if(!Path.exists(path))
					throw new FsException(FileNotFound, path);
				try {
					FilePath.ofString(Path.realpath(path));
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static function pyOpenFlags(flag:FileOpenFlag<Dynamic>, binary:Bool):Int {
		var flags = switch flag {
			case Append: Os.O_WRONLY | Os.O_APPEND | Os.O_CREAT;
			case Read: Os.O_RDONLY;
			case ReadWrite: Os.O_RDWR;
			case Write: Os.O_WRONLY | Os.O_CREAT | Os.O_TRUNC;
			case WriteX: Os.O_WRONLY | Os.O_CREAT | Os.O_EXCL;
			case WriteRead: Os.O_RDWR | Os.O_CREAT | Os.O_TRUNC;
			case WriteReadX: Os.O_RDWR | Os.O_CREAT | Os.O_EXCL;
			case Overwrite: Os.O_WRONLY | Os.O_CREAT;
			case OverwriteRead: Os.O_RDWR | Os.O_CREAT;
		}
		return binary && Os.name == 'nt' ? Os.O_BINARY | flags : flags;
	}
}
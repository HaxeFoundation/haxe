package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import php.Global.*;
import php.Syntax;
import php.NativeArray;
import php.NativeIndexedArray;
import php.Resource;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

@:coreApi
class FileSystem {
	static inline function run(job:()->Void):Void {
		inline haxe.EntryPoint.runInMainThread(job);
	}

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		run(() -> {
			var result = try {
				cast @:privateAccess new File(fopenHx(path, flag), path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function tempFile(callback:Callback<File>):Void {
		run(() -> {
			var result = try {
				switch tmpfile() {
					case false:
						throw new php.Exception('Failed to create a temporary file');
					case fd:
						@:privateAccess new File(fd, stream_get_meta_data(fd)['uri']);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), '(unknown path)'));
				return;
			}
			callback.success(result);
		});
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		run(() -> {
			var result = try {
				switch file_get_contents(path) {
					case false:
						throw new php.Exception('Failed to read a file');
					case r:
						Bytes.ofString(r);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		run(() -> {
			var result = try {
				switch file_get_contents(path) {
					case false:
						throw new php.Exception('Failed to read a file');
					case r:
						r;
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				var f = fopenHx(path, flag);
				fwrite(f, data.getData().toString());
				fclose(f);
				NoData.NoData;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				var f = fopenHx(path, flag);
				fwrite(f, text);
				fclose(f);
				NoData.NoData;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		run(() -> {
			var result = try {
				switch opendir(path) {
					case false:
						throw new php.Exception('Failed to open a directory');
					case result:
						@:privateAccess new Directory(result, path, maxBatchSize);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		run(() -> {
			var result = try {
				switch scandir(path) {
					case false:
						throw new php.Exception('Failed to list a directory');
					case (_:NativeIndexedArray<String>) => list:
						[for(item in list) if(item != '.' && item != '..') (item:FilePath)];
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		if(permissions == null) permissions = 511;
		run(() -> {
			var result = try {
				if(mkdir(path, permissions, recursive))
					NoData.NoData
				else
					throw new php.Exception('Failed to create a directory');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		if(permissions == null) permissions = 511;
		run(() -> {
			var result = try {
				prefix = (prefix == null ? '' : prefix) + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
				var path:String = rtrim(parentDirectory, FilePath.SEPARATOR == '/' ? '/' : '\\/') + FilePath.SEPARATOR + prefix;
				while(true) {
					try {
						if(mkdir(path, permissions, recursive))
							break
						else
							throw new php.Exception('Failed to create a directory');
					} catch(e:php.Exception) {
						switch strpos(e.getMessage(), 'mkdir(): File exists') {
							case false:
								throw e;
							case _:
								path += getRandomChar();
						}
					}
				}
				(path:FilePath);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), parentDirectory));
				return;
			}
			callback.success(result);
		});
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
		run(() -> {
			if(!overwrite && file_exists(newPath)) {
				callback.fail(new FsException(FileExists, newPath));
				return;
			}
			var result = try {
				if(rename(oldPath, newPath))
					NoData
				else
					throw new php.Exception('Failed to move file or directory');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), oldPath));
				return;
			}
			callback.success(result);
		});
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(unlink(path))
					NoData.NoData
				else
					throw new php.Exception('Failed to delete a file');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(rmdir(path))
					NoData.NoData
				else
					throw new php.Exception('Failed to delete a file');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		run(() -> {
			var result = try {
				switch stat(path) {
					case false:
						throw new php.Exception('Failed to stat');
					case result:
						@:privateAccess FileSystem.phpStatToHx(result);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		run(() -> {
			var result = try {
				(!mode.has(Exists) || file_exists(path))
				&& (!mode.has(Readable) || is_readable(path))
				&& (!mode.has(Writable) || is_writable(path))
				&& (!mode.has(Executable) || is_executable(path));
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		run(() -> {
			var result = try {
				is_dir(path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		run(() -> {
			var result = try {
				is_file(path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(chmod(path, permissions))
					NoData.NoData
				else
					throw new php.Exception('Failed to set permissions');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(chown(path, user) && chgrp(path, group))
					NoData.NoData
				else
					throw new php.Exception('Failed to set owner');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(lchown(path, user) && lchgrp(path, group))
					NoData.NoData
				else
					throw new php.Exception('Failed to set owner');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				var success = switch type {
					case SymLink: symlink(target, path);
					case HardLink: php.Global.link(target, path);
				}
				if(success)
					NoData.NoData
				else
					throw new php.Exception('Failed to create a link');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		run(() -> {
			var result = try {
				is_link(path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		run(() -> {
			var result = try {
				switch readlink(path) {
					case false:
						throw new php.Exception('Failed to read a link');
					case (_:String) => r:
						if(FilePath.SEPARATOR == '\\' && !is_link(path))
							throw new php.Exception('Failed to read a link');
						(r:FilePath);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		run(() -> {
			var result = try {
				switch lstat(path) {
					case false:
						throw new php.Exception('Failed to stat');
					case result:
						@:privateAccess FileSystem.phpStatToHx(result);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		run(() -> {
			if(!overwrite && file_exists(destination)) {
				callback.fail(new FsException(FileExists, destination));
				return;
			}
			var result = try {
				if(copy(source, destination))
					NoData.NoData
				else
					throw new php.Exception('Failed to copy a file');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), source));
				return;
			}
			callback.success(result);
		});
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				var f = fopen(path, 'a');
				var success = ftruncate(f, newSize);
				fclose(f);
				if(success)
					NoData.NoData
				else
					throw new php.Exception('Failed to resize file');
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		run(() -> {
			var result = try {
				if(file_exists(path)) {
					if(touch(path, modificationTime, accessTime))
						NoData.NoData
					else
						throw new php.Exception('Failed to set file times');
				} else {
					throw new php.Exception('No such file');
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		run(() -> {
			var result = try {
				switch realpath(path) {
					case false:
						throw new php.Exception('Unable to resolve real path');
					case (_:String) => r:
						(r:FilePath);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static function fopenHx(file:String, flag:FileOpenFlag<Dynamic>):Resource {
		var f = switch flag {
			case Append: fopen(file, 'a');
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
			throw new php.Exception('Cannot open file');
		return f;
	}

	static function phpStatToHx(phpStat:NativeArray):FileInfo {
		return {
			atime: phpStat['atime'],
			mtime: phpStat['mtime'],
			ctime: phpStat['ctime'],
			dev: phpStat['dev'],
			gid: phpStat['gid'],
			uid: phpStat['uid'],
			ino: phpStat['ino'],
			mode: phpStat['mode'],
			nlink: phpStat['nlink'],
			rdev: phpStat['rdev'],
			size: phpStat['size'],
			blksize: phpStat['blksize'],
			blocks: phpStat['blocks']
		}
	}
}
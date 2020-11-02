package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.IJobExecutor;
import php.Global.*;
import php.Syntax;
import php.NativeArray;
import php.NativeIndexedArray;
import php.Resource;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

/**
	File system operations.
**/
@:coreApi
class FileSystem {
	static public dynamic function create(executor:IJobExecutor = null):IFileSystem {
		return new DefaultFileSystem(executor == null ? Native.defaultExecutor : executor);
	}

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).openFile(path, flag, callback);

	static public function tempFile(callback:Callback<File>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).tempFile(callback);

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).readBytes(path, callback);

	static public function readString(path:FilePath, callback:Callback<String>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).readString(path, callback);

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).writeBytes(path, data, flag, callback);

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).writeString(path, text, flag, callback);

	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).openDirectory(path, callback);

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).listDirectory(path, callback);

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).uniqueDirectory(parentDirectory, prefix, permissions, recursive, callback);

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).move(oldPath, newPath, overwrite, callback);

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).deleteFile(path, callback);

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).deleteDirectory(path, callback);

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).info(path, callback);

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).check(path, mode, callback);

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).isDirectory(path, callback);

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).isFile(path, callback);

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).setPermissions(path, permissions, callback);

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).setOwner(path, user, group, callback);

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).setLinkOwner(path, user, group, callback);

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).link(target, path, type, callback);

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).isLink(path, callback);

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).readLink(path, callback);

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).linkInfo(path, callback);

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).copyFile(source, destination, overwrite, callback);

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).resize(path, newSize, callback);

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).setTimes(path, accessTime, modificationTime, callback);

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).realPath(path, callback);

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

class DefaultFileSystem implements IFileSystem {
	final jobs:IJobExecutor;

	public function new(executor:IJobExecutor) {
		jobs = executor;
	}

	public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		jobs.addJob(
			() -> {
				try {
					cast new File(fopenHx(path, flag), path, jobs);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function tempFile(callback:Callback<File>):Void {
		jobs.addJob(
			() -> {
				try {
					switch tmpfile() {
						case false:
							throw new php.Exception('Failed to create a temporary file');
						case fd:
							new File(fd, stream_get_meta_data(fd)['uri'], jobs);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), '(unknown path)');
				}
			},
			callback
		);
	}

	public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		jobs.addJob(
			() -> {
				try {
					switch file_get_contents(path) {
						case false:
							throw new FsException(CustomError('Failed to read a file'), path);
						case r:
							Bytes.ofString(r);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function readString(path:FilePath, callback:Callback<String>):Void {
		jobs.addJob(
			() -> {
				try {
					switch file_get_contents(path) {
						case false:
							throw new FsException(CustomError('Failed to read a file'), path);
						case r:
							r;
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopenHx(path, flag);
					fwrite(f, data.getData().toString());
					fclose(f);
					NoData.NoData;
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopenHx(path, flag);
					fwrite(f, text);
					fclose(f);
					NoData.NoData;
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		jobs.addJob(
			() -> {
				try {
					switch opendir(path) {
						case false:
							throw new php.Exception('Failed to open a directory');
						case result:
							@:privateAccess new Directory(result, path, jobs);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		jobs.addJob(
			() -> {
				try {
					switch scandir(path) {
						case false:
							throw new php.Exception('Failed to list a directory');
						case (_:NativeIndexedArray<String>) => list:
							[for(item in list) if(item != '.' && item != '..') (item:FilePath)];
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function createDirectory(path:FilePath, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(mkdir(path, permissions, recursive))
						NoData.NoData
					else
						throw new php.Exception('Failed to create a directory');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
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
					throw new FsException(CustomError(e.getMessage()), parentDirectory);
				}
			},
			callback
		);
	}

	static var __codes:Null<Array<String>>;
	static function getRandomChar():String {
		//TODO: null safety issue if `switch` result is assigned directly to this var declaration
		var codes:Array<String>;
		switch __codes {
			case null:
				var a = [for(c in '0'.code...'9'.code) String.fromCharCode(c)];
				for(c in 'A'.code...'Z'.code) a.push(String.fromCharCode(c));
				for(c in 'a'.code...'z'.code) a.push(String.fromCharCode(c));
				codes = __codes = a;
			case a:
				codes = a;
		}
		return codes[Std.random(codes.length)];
	}

	public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				if(!overwrite && file_exists(newPath))
					throw new FsException(FileExists, newPath);
				try {
					if(rename(oldPath, newPath))
						NoData
					else
						throw new FsException(CustomError('Failed to move file or directory'), oldPath);
				} catch(e:FsException) {
					throw e;
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), oldPath);
				}
			},
			callback
		);
	}

	public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(unlink(path))
						NoData.NoData
					else
						throw new php.Exception('Failed to delete a file');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(rmdir(path))
						NoData.NoData
					else
						throw new php.Exception('Failed to delete a file');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					switch stat(path) {
						case false:
							throw new php.Exception('Failed to stat');
						case result:
							@:privateAccess FileSystem.phpStatToHx(result);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					(!mode.has(Exists) || file_exists(path))
					&& (!mode.has(Readable) || is_readable(path))
					&& (!mode.has(Writable) || is_writable(path))
					&& (!mode.has(Executable) || is_executable(path));
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_dir(path);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_file(path);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(chmod(path, permissions))
						NoData.NoData
					else
						throw new php.Exception('Failed to set permissions');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(chown(path, user) && chgrp(path, group))
						NoData.NoData
					else
						throw new php.Exception('Failed to set owner');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(lchown(path, user) && lchgrp(path, group))
						NoData.NoData
					else
						throw new php.Exception('Failed to set owner');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var success = switch type {
						case SymLink: symlink(target, path);
						case HardLink: php.Global.link(target, path);
					}
					if(success)
						NoData.NoData
					else
						throw new php.Exception('Failed to create a link');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_link(path);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					switch readlink(path) {
						case false:
							throw new php.Exception('Failed to read a link');
						case (_:String) => r:
							(r:FilePath);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					switch lstat(path) {
						case false:
							throw new php.Exception('Failed to stat');
						case result:
							@:privateAccess FileSystem.phpStatToHx(result);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				if(!overwrite && file_exists(destination))
					throw new FsException(FileExists, destination);
				try {
					if(copy(source, destination))
						NoData.NoData
					else
						throw new php.Exception('Failed to copy a file');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), source);
				}
			},
			callback
		);
	}

	public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopen(path, 'a');
					var success = ftruncate(f, newSize);
					fclose(f);
					if(success)
						NoData.NoData
					else
						throw new php.Exception('Failed to resize file');
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(file_exists(path)) {
						if(touch(path, modificationTime, accessTime))
							NoData.NoData
						else
							throw new php.Exception('Failed to set file times');
					} else {
						throw new php.Exception('No such file');
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					switch realpath(path) {
						case false:
							throw new php.Exception('Unable to resolve real path');
						case (_:String) => r:
							(r:FilePath);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
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
}
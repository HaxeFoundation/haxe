package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import haxe.NoData;
import haxe.IJobExecutor;
import php.Global.*;
import php.Syntax;
import php.NativeArray;
import php.NativeIndexedArray;
import php.Resource;

/**
	File system operations.
**/
@:coreApi
class FileSystem {
	static public function create(executor:IJobExecutor = null):IFileSystem {
		return new FileSystemImpl(executor == null ? Native.defaultExecutor : executor);
	}

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void
		new FileSystemImpl(Native.defaultExecutor).openFile(path, flag, callback);

	static public function tempFile(callback:Callback<File>):Void
		new FileSystemImpl(Native.defaultExecutor).tempFile(callback);

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void
		new FileSystemImpl(Native.defaultExecutor).readBytes(path, callback);

	static public function readString(path:FilePath, callback:Callback<String>):Void
		new FileSystemImpl(Native.defaultExecutor).readString(path, callback);

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).writeBytes(path, data, flag, callback);

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).writeString(path, text, flag, callback);

	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void
		new FileSystemImpl(Native.defaultExecutor).openDirectory(path, callback);

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void
		new FileSystemImpl(Native.defaultExecutor).listDirectory(path, callback);

	static public function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(prefix:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).uniqueDirectory(prefix, permissions, recursive, callback);

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).move(oldPath, newPath, overwrite, callback);

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).deleteFile(path, callback);

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).deleteDirectory(path, callback);

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void
		new FileSystemImpl(Native.defaultExecutor).info(path, callback);

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).check(path, mode, callback);

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isDirectory(path, callback);

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isFile(path, callback);

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setPermissions(path, permissions, callback);

	static public function setOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setOwner(path, userId, groupId, callback);

	static public function setLinkOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setLinkOwner(path, userId, groupId, callback);

	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).link(target, path, type, callback);

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isLink(path, callback);

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).readLink(path, callback);

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void
		new FileSystemImpl(Native.defaultExecutor).linkInfo(path, callback);

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).copyFile(source, destination, overwrite, callback);

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).resize(path, newSize, callback);

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setTimes(path, accessTime, modificationTime, callback);

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


private class FileSystemImpl implements IFileSystem {
	final jobs:IJobExecutor;

	public inline function new(jobs:IJobExecutor) {
		this.jobs = jobs;
	}

	public inline function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		jobs.addJob(
			() -> {
				try {
					cast @:privateAccess new File(fopenHx(path, flag), path);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function tempFile(callback:Callback<File>):Void {
		jobs.addJob(
			() -> {
				try {
					switch tmpfile() {
						case false:
							throw new php.Exception('Failed to create a temporary file');
						case fd:
							@:privateAccess new File(fd, stream_get_meta_data(fd)['uri']);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), '(unknown path)');
				}
			},
			callback
		);
	}

	public inline function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		jobs.addJob(
			() -> {
				try {
					switch file_get_contents(path.phpStr()) {
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

	public inline function readString(path:FilePath, callback:Callback<String>):Void {
		jobs.addJob(
			() -> {
				try {
					switch file_get_contents(path.phpStr()) {
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

	public inline function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopenHx(path.phpStr(), flag);
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

	public inline function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopenHx(path.phpStr(), flag);
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

	public inline function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		jobs.addJob(
			() -> {
				try {
					switch opendir(path.phpStr()) {
						case false:
							throw new php.Exception('Failed to open a directory');
						case result:
							@:privateAccess new Directory(result, path);
					}
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		jobs.addJob(
			() -> {
				try {
					switch scandir(path.phpStr()) {
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

	public inline function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(mkdir(path.phpStr(), permissions, recursive))
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

	public inline function uniqueDirectory(prefix:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					var path:String = prefix.phpStr() + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
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
					throw new FsException(CustomError(e.getMessage()), prefix);
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

	// TODO:
	// This implementation is wrong. It will fail if any entry of a moved directory is not allowed to move
	// (e.g. because of permissions)
	public inline function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				if(!overwrite && file_exists(newPath.phpStr()))
					throw new FsException(FileExists, newPath);
				try {
					if(moveRecursive(oldPath.phpStr(), newPath.phpStr()))
						NoData.NoData
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

	public inline function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(unlink(path.phpStr()))
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

	public inline function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(rmdir(path.phpStr()))
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

	public inline function info(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					switch stat(path.phpStr()) {
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

	public inline function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					(!mode.has(Exists) || file_exists(path.phpStr()))
					&& (!mode.has(Readable) || is_readable(path.phpStr()))
					&& (!mode.has(Writable) || is_writable(path.phpStr()))
					&& (!mode.has(Executable) || is_executable(path.phpStr()));
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_dir(path.phpStr());
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function isFile(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_file(path.phpStr());
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(chmod(path.phpStr(), permissions))
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

	public inline function setOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(chown(path.phpStr(), userId) && chgrp(path.phpStr(), groupId))
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

	public inline function setLinkOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(lchown(path.phpStr(), userId) && lchgrp(path.phpStr(), groupId))
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

	public inline function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		var path:FilePath = path == null ? basename(target.phpStr()) : path;
		jobs.addJob(
			() -> {
				try {
					var success = switch type {
						case SymLink: symlink(target.phpStr(), path.phpStr());
						case HardLink: php.Global.link(target.phpStr(), path.phpStr());
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

	public inline function isLink(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					is_link(path.phpStr());
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public inline function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					switch readlink(path.phpStr()) {
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

	public inline function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					switch lstat(path.phpStr()) {
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

	public inline function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				if(!overwrite && file_exists(destination.phpStr()))
					throw new FsException(FileExists, destination);
				try {
					if(copy(source.phpStr(), destination.phpStr()))
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

	public inline function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = fopen(path.phpStr(), 'r+');
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

	public inline function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(touch(path.phpStr(), modificationTime, accessTime))
						NoData.NoData
					else
						throw new php.Exception('Failed to set file times');
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
			throw new php.Exception('Cannot open file');
		return f;
	}
}
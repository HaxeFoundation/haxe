package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.IJobExecutor;
import haxe.ValueException;
import eval.Unix;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

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

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).uniqueDirectory(parentDirectory, prefix, permissions, recursive, callback);

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

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setOwner(path, user, group, callback);

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setLinkOwner(path, user, group, callback);

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void
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

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).realPath(path, callback);
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
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function tempFile(callback:Callback<File>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), '(unknown path)');
				}
			},
			callback
		);
	}

	public inline function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function readString(path:FilePath, callback:Callback<String>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function createDirectory(path:FilePath, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), parentDirectory);
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

	public inline function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), oldPath);
				}
			},
			callback
		);
	}

	public inline function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function info(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function isFile(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function isLink(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					try {
						Unix.isLink(path);
					} catch(err:PosixError) {
						switch err {
							case ENOENT: false;
							case _: throw err;
						}
					}
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), source);
				}
			},
			callback
		);
	}

	public inline function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}

	public inline function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					throw new haxe.exceptions.NotImplementedException();
				} catch(e) {
					throw new FsException(CustomError(e.message), path);
				}
			},
			callback
		);
	}
}
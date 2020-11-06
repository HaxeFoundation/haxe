package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.NoData;
import haxe.IJobExecutor;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import sys.thread.Thread;
import eval.integers.Int64;
import eval.integers.UInt64;
import eval.luv.Loop;
import eval.luv.Buffer;
import eval.luv.File as LFile;
import eval.luv.Dir;
import eval.luv.File.FileOpenFlag as LFileOpenFlag;
import eval.luv.File.FileAccessFlag;
import eval.luv.LuvException;

using eval.luv.Result;

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

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void
		inline (inline new DefaultFileSystem(Native.defaultExecutor)).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void
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
}

class DefaultFileSystem implements IFileSystem {

	static inline function currentLoop():Loop {
		return Thread.current().events;
	}

	public function new(_:IJobExecutor) {
		//executor is not used in this implementation
	}

	public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function tempFile(callback:Callback<File>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		readFile(path, callback);
	}

	public function readString(path:FilePath, callback:Callback<String>):Void {
		readFile(path, (e, r) -> {
			if(e == null)
				callback.success(r.toString())
			else
				callback.fail(e);
		});
	}

	inline function readFile(path:FilePath, callback:Callback<Bytes>) {
		var loop = currentLoop();
		LFile.open(loop, path, [RDONLY], r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(f):
				f.fstat(loop, null, r -> switch r {
					case Error(e):
						f.close(loop, null, _ -> callback.fail(new FsException(e, path)));
					case Ok(stat):
						var buf = Buffer.create(stat.size.toInt());
						f.read(loop, Int64.ZERO, [buf], r -> switch r {
							case Error(e):
								f.close(loop, null, _ -> callback.fail(new FsException(e, path)));
							case Ok(bytesRead):
								f.close(loop, null, _ -> callback.success(buf.sub(0, bytesRead.toInt()).toBytes()));
						});
				});
		});
	}

	public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeFile(path, data, flag, callback);
	}

	public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeFile(path, text, flag, callback);
	}

	inline function writeFile(path:FilePath, data:Buffer, flag:FileOpenFlag<Dynamic>, callback:Callback<NoData>) {
		var loop = currentLoop();
		LFile.open(loop, path, evalOpenFlags(flag), r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(f):
				f.write(loop, Int64.ZERO, [data], r -> switch r {
					case Error(e):
						f.close(loop, null, _ -> callback.fail(new FsException(e, path)));
					case Ok(_):
						f.close(loop, null, r -> switch r {
							case Error(e): callback.fail(new FsException(e, path));
							case Ok(_): callback.success(NoData);
						});
				});
		});
	}

	public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		var loop = currentLoop();
		Dir.open(loop, path, null, r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(dir):
				var result = [];
				function collect(r:Result<Array<Dirent>>) {
					switch r {
						case Error(e):
							dir.close(loop, null, _ -> callback.fail(new FsException(e, path)));
						case Ok(entries):
							if(entries.length == 0) {
								dir.close(loop, null, _ -> callback.success(result));
							} else {
								for(entry in entries) {
									result.push(@:privateAccess new FilePath(entry.name));
								}
								dir.read(loop, 32, null, collect);
							}
					}
				}
				dir.read(loop, 32, null, collect);
		});
	}

	public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		if(permissions == null) permissions = 511;
		var loop = currentLoop();
		LFile.mkdir(loop, path, permissions, null, r -> switch r {
			case Error(UV_ENOENT):
				callback.success(NoData);
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(_):
				callback.success(NoData);
		});
	}

	public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		if(permissions == null) permissions = 511;
		throw new haxe.exceptions.NotImplementedException();
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
		throw new haxe.exceptions.NotImplementedException();
	}

	public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		LFile.unlink(currentLoop(), path, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		LFile.stat(currentLoop(), path, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success(stat);
		});
	}

	public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		var flags = [];
		if(mode.has(Exists)) flags.push(F_OK);
		if(mode.has(Executable)) flags.push(X_OK);
		if(mode.has(Writable)) flags.push(W_OK);
		if(mode.has(Readable)) flags.push(R_OK);
		LFile.access(currentLoop(), path, flags, null, r -> switch r {
			case Error(UV_ENOENT | UV_EACCES): callback.success(false);
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(true);
		});
	}

	public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		LFile.stat(currentLoop(), path, null, r -> switch r {
			case Error(UV_ENOENT): callback.success(false);
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success((stat:FileInfo).mode.isDirectory());
		});
	}

	public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		LFile.stat(currentLoop(), path, null, r -> switch r {
			case Error(UV_ENOENT): callback.success(false);
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success((stat:FileInfo).mode.isFile());
		});
	}

	public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		LFile.chmod(currentLoop(), path, [NUMERIC(permissions)], null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		LFile.chown(currentLoop(), path, user, group, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		LFile.lchown(currentLoop(), path, user, group, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}

	public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		var cb:(r:Result<NoData>)->Void = r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		}
		switch type {
			case HardLink:
				LFile.link(currentLoop(), target, path, null, cb);
			case SymLink:
				LFile.symlink(currentLoop(), target, path, null, null, cb);
		}
	}

	public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		LFile.lstat(currentLoop(), path, null, r -> switch r {
			case Error(UV_ENOENT): callback.success(false);
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success((stat:FileInfo).mode.isLink());
		});
	}

	public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		LFile.readLink(currentLoop(), path, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(real): callback.success(@:privateAccess new FilePath(real));
		});
	}

	public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		LFile.lstat(currentLoop(), path, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(stat): callback.success(stat);
		});
	}

	public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		LFile.copyFile(currentLoop(), source, destination, (overwrite ? null : [COPYFILE_EXCL]), null, r -> switch r {
			case Error(UV_EEXIST): callback.fail(new FsException(FileExists, destination));
			case Error(e): callback.fail(new FsException(e, source));
			case Ok(stat): callback.success(stat);
		});
	}

	public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		var loop = currentLoop();
		LFile.open(loop, path, [CREAT, WRONLY], null, null, r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(file):
				file.ftruncate(loop, Int64.ofInt(newSize), null, r -> switch r {
					case Error(e):
						file.close(loop, null, _ -> callback.fail(new FsException(e, path)));
					case Ok(_):
						file.close(loop, null, r -> switch r {
							case Error(e):
								callback.fail(new FsException(e, path));
							case Ok(_):
								callback.success(NoData);
						});
				});
		});
	}

	public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		LFile.utime(currentLoop(), path, accessTime, modificationTime, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(real): callback.success(NoData);
		});
	}

	public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		LFile.realPath(currentLoop(), path, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(real): callback.success(@:privateAccess new FilePath(real));
		});
	}

	static function evalOpenFlags(flag:FileOpenFlag<Dynamic>):Array<LFileOpenFlag> {
		return switch flag {
			case Append: [WRONLY, APPEND, CREAT];
			case Read: [RDONLY];
			case ReadWrite: [RDWR];
			case Write: [WRONLY, CREAT, TRUNC];
			case WriteX: [WRONLY, CREAT, EXCL];
			case WriteRead: [RDWR, TRUNC, CREAT];
			case WriteReadX: [RDWR, CREAT, EXCL];
			case Overwrite: [WRONLY, CREAT];
			case OverwriteRead: [RDWR, CREAT];
		}
	}
}
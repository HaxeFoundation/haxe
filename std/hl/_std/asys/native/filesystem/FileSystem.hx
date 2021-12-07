package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.io.BytesData;
import haxe.NoData;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import sys.thread.Thread;
import hl.I64;
import hl.uv.UVError;
import hl.uv.Loop;
import hl.Bytes as HlBytes;
import hl.uv.File as LFile;
import hl.uv.Dir;
import hl.uv.File.FileOpenFlag as LFileOpenFlag;
import hl.uv.File.FileAccessMode as LFileAccessMode;

@:coreApi
class FileSystem {
	@:allow(asys.native.filesystem)
	static inline function currentLoop():Loop {
		return Thread.current().events;
	}

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		LFile.open(currentLoop(), path, hlOpenFlags(flag), (e,f) -> switch e {
			case UV_NOERR: callback.success(cast @:privateAccess new File(f, path));
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function tempFile(callback:Callback<File>):Void {
		var pattern = hl.uv.Misc.tmpDir() + '/XXXXXX';
		LFile.mkstemp(currentLoop(), pattern, (e, f, path) -> switch e {
			case UV_NOERR: callback.success(@:privateAccess new File(f, @:privateAccess new FilePath(path), true));
			case _: callback.fail(new FsException(ioError(e), '(unknown path)'));
		});
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		readFile(path, callback);
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		readFile(path, (e, r) -> {
			if(e == null)
				callback.success(r.toString())
			else
				callback.fail(e);
		});
	}

	static inline function readFile(path:FilePath, callback:Callback<Bytes>):Void {
		var loop = currentLoop();
		LFile.open(loop, path, [O_RDONLY], (e, f) -> switch e {
			case UV_NOERR:
				f.fstat(loop, (e, stat) -> switch e {
					case UV_NOERR:
						var length = stat.size.toInt();
						var buf = new HlBytes(length);
						f.read(loop, buf, length, I64.ofInt(0), (e, bytesRead) -> switch e {
							case UV_NOERR:
								var bytesRead = bytesRead.toInt();
								f.close(loop, _ -> callback.success(Bytes.ofData(new BytesData(buf.sub(0, bytesRead), bytesRead))));
							case _:
								f.close(loop, _ -> callback.fail(new FsException(ioError(e), path)));
						});
					case _:
						f.close(loop, _ -> callback.fail(new FsException(ioError(e), path)));
				});
			case _:
				callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeFile(path, data.getData().bytes, data.length, flag, callback);
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		var length = 0;
		var utf8 = @:privateAccess text.bytes.utf16ToUtf8(0, length);
		writeFile(path, utf8, length, flag, callback);
	}

	static inline function writeFile(path:FilePath, data:HlBytes, length:Int, flag:FileOpenFlag<Dynamic>, callback:Callback<NoData>):Void {
		var loop = currentLoop();
		LFile.open(loop, path, hlOpenFlags(flag), (e,f) -> switch e {
			case UV_NOERR:
				f.write(loop, data, length, I64.ofInt(0), (e, _) -> switch e {
					case UV_NOERR:
						f.close(loop, e -> switch e {
							case UV_NOERR: callback.success(NoData);
							case _: callback.fail(new FsException(ioError(e), path));
						});
					case _:
						f.close(loop, _ -> callback.fail(new FsException(ioError(e), path)));
				});
			case _:
				callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		Dir.open(currentLoop(), path, (e, dir) -> switch e {
			case UV_NOERR: callback.success(@:privateAccess new Directory(dir, path, maxBatchSize));
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		var loop = currentLoop();
		Dir.open(loop, path, (e, dir) -> switch e {
			case UV_NOERR:
				var result = [];
				function collect(e:UVError, entries:Null<Array<DirEntry>>) {
					switch e {
						case UV_NOERR:
							if(entries.length == 0) {
								dir.close(loop, _ -> callback.success(result));
							} else {
								for(entry in entries)
									result.push(@:privateAccess new FilePath(entry.name));
								dir.read(loop, 32, collect);
							}
						case _:
							dir.close(loop, _ -> callback.fail(new FsException(ioError(e), path)));
					}
				}
				dir.read(loop, 32, collect);
			case _:
				callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		if(permissions == null) permissions = 511;
		inline mkdir(path, permissions, recursive, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static function mkdir(path:FilePath, permissions:FilePermissions, recursive:Bool, callback:(e:UVError)->Void):Void {
		var loop = currentLoop();
		function mk(path:FilePath, callback:(e:UVError)->Void) {
			LFile.mkdir(loop, path, permissions, e -> switch e {
				case UV_ENOENT if(recursive):
					switch path.parent() {
						case null:
							callback(e);
						case parent:
							mk(parent, e -> switch e {
								case UV_NOERR:
									LFile.mkdir(loop, path, permissions, callback);
								case _:
									callback(e);
							});
					}
				case _:
					callback(e);
			});
		}
		mk(path, callback);
	}

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		if(permissions == null) permissions = 511;

		var name = (prefix == null ? '' : prefix) + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
		var path = @:privateAccess new FilePath(parentDirectory.add(name));

		function create(callback:(e:UVError)->Void) {
			inline mkdir(path, permissions, recursive, e -> switch e {
				case UV_EEXIST:
					var next = (path:String) + getRandomChar();
					path = @:privateAccess new FilePath(next);
					create(callback);
				case _:
					callback(e);
			});
		}
		create(e -> switch e {
			case UV_NOERR: callback.success(path);
			case _: callback.fail(new FsException(ioError(e), parentDirectory));
		});
	}

	static var __codes:Null<Array<String>>;
	static function getRandomChar():String {
		if(__codes == null) {
			var a = [for(c in '0'.code...'9'.code) String.fromCharCode(c)];
			for(c in 'A'.code...'Z'.code) a.push(String.fromCharCode(c));
			for(c in 'a'.code...'z'.code) a.push(String.fromCharCode(c));
			__codes = a;
		}
		return __codes[Std.random(__codes.length)];
	}

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		var loop = currentLoop();
		inline function move() {
			LFile.rename(loop, oldPath, newPath, e -> switch e {
				case UV_NOERR: callback.success(NoData);
				case _: callback.fail(new FsException(ioError(e), oldPath));
			});
		}
		if(overwrite) {
			move();
		} else {
			LFile.access(loop, newPath, [F_OK], e -> switch e {
				case UV_ENOENT: move();
				case UV_NOERR: callback.fail(new FsException(FileExists, newPath));
				case _: callback.fail(new FsException(ioError(e), newPath));
			});
		}
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		LFile.unlink(currentLoop(), path, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		LFile.rmdir(currentLoop(), path, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case e: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		LFile.stat(currentLoop(), path, (e, stat) -> switch e {
			case UV_NOERR: callback.success(stat);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		var flags = [];
		if(mode.has(Exists)) flags.push(F_OK);
		if(mode.has(Executable)) flags.push(X_OK);
		if(mode.has(Writable)) flags.push(W_OK);
		if(mode.has(Readable)) flags.push(R_OK);
		LFile.access(currentLoop(), path, flags, e -> switch e {
			case UV_ENOENT | UV_EACCES: callback.success(false);
			case UV_NOERR: callback.success(true);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		LFile.stat(currentLoop(), path, (e, stat) -> switch e {
			case UV_ENOENT: callback.success(false);
			case UV_NOERR: callback.success((stat:FileInfo).mode.isDirectory());
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		LFile.stat(currentLoop(), path, (e, stat) -> switch e {
			case UV_ENOENT: callback.success(false);
			case UV_NOERR: callback.success((stat:FileInfo).mode.isFile());
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		LFile.chmod(currentLoop(), path, permissions, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		LFile.chown(currentLoop(), path, user, group, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		LFile.lchown(currentLoop(), path, user, group, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		var cb = e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		}
		switch type {
			case HardLink:
				LFile.link(currentLoop(), target, path, cb);
			case SymLink:
				LFile.symlink(currentLoop(), target, path, null, cb);
		}
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		LFile.lstat(currentLoop(), path, (e, stat) -> switch e {
			case UV_ENOENT: callback.success(false);
			case UV_NOERR: callback.success((stat:FileInfo).mode.isLink());
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		LFile.readLink(currentLoop(), path, (e, real) -> switch e {
			case UV_NOERR: callback.success(@:privateAccess new FilePath(real));
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		LFile.lstat(currentLoop(), path, (e, stat) -> switch e {
			case UV_NOERR: callback.success(stat);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		LFile.copyFile(currentLoop(), source, destination, (overwrite ? null : [EXCL]), e -> switch e {
			case UV_EEXIST: callback.fail(new FsException(FileExists, destination));
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), source));
		});
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		var loop = currentLoop();
		LFile.open(loop, path, [O_CREAT(420), O_WRONLY], (e, file) -> switch e {
			case UV_NOERR:
				file.ftruncate(loop, I64.ofInt(newSize), e -> switch e {
					case UV_NOERR:
						file.close(loop, e -> switch e {
							case UV_NOERR:
								callback.success(NoData);
							case _:
								callback.fail(new FsException(ioError(e), path));
						});
					case _:
						file.close(loop, _ -> callback.fail(new FsException(ioError(e), path)));
				});
			case _:
				callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		LFile.utime(currentLoop(), path, accessTime, modificationTime, e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		LFile.realPath(currentLoop(), path, (e, real) -> switch e {
			case UV_NOERR: callback.success(@:privateAccess new FilePath(real));
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}

	// mode 420 == 0644
	static function hlOpenFlags(flag:FileOpenFlag<Dynamic>, mode:Int = 420):Array<LFileOpenFlag> {
		return switch flag {
			case Append: [O_WRONLY, O_APPEND, O_CREAT(mode)];
			case Read: [O_RDONLY];
			case ReadWrite: [O_RDWR];
			case Write: [O_WRONLY, O_CREAT(mode), O_TRUNC];
			case WriteX: [O_WRONLY, O_CREAT(mode), O_EXCL];
			case WriteRead: [O_RDWR, O_CREAT(mode), O_TRUNC];
			case WriteReadX: [O_RDWR, O_CREAT(mode), O_EXCL];
			case Overwrite: [O_WRONLY, O_CREAT(mode)];
			case OverwriteRead: [O_RDWR, O_CREAT(mode)];
		}
	}

	@:allow(asys.native.filesystem)
	static function ioError(e:UVError):Null<IoErrorType> {
		return switch e {
			case UV_NOERR: null;
			case UV_ENOENT: FileNotFound;
			case UV_EEXIST: FileExists;
			case UV_ESRCH: ProcessNotFound;
			case UV_EACCES: AccessDenied;
			case UV_ENOTDIR: NotDirectory;
			case UV_EMFILE: TooManyOpenFiles;
			case UV_EPIPE: BrokenPipe;
			case UV_ENOTEMPTY: NotEmpty;
			case UV_EADDRNOTAVAIL: AddressNotAvailable;
			case UV_ECONNRESET: ConnectionReset;
			case UV_ETIMEDOUT: TimedOut;
			case UV_ECONNREFUSED: ConnectionRefused;
			case UV_EBADF: BadFile;
			case _: CustomError(e.toString());
		}
	}
}
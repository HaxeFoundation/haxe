package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import cs.NativeArray;
import cs.system.Exception as CsException;
import sys.thread.ElasticThreadPool;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import cs.system.io.File as CsFile;
import cs.system.io.Directory as CsDirectory;
import cs.system.io.FileMode;
import cs.system.io.FileAccess;
import cs.system.io.FileStream;
import cs.system.io.FileAttributes;
import cs.system.io.FileInfo as CsFileInfo;
import cs.system.io.DirectoryInfo;
import cs.system.io.FileNotFoundException;
import cs.system.io.DirectoryNotFoundException;
import cs.system.security.SecurityException;
import cs.system.text.Encoding.UTF8;
import cs.StdTypes.UInt8;
import cs.system.DateTime;
import cs.system.DateTimeKind;
import cs.system.DateTimeOffset;
import cs.system.Guid;
import cs.system.security.accesscontrol.FileSystemRights;
import cs.system.security.accesscontrol.AccessControlType;
import cs.system.security.PermissionSet;
import cs.system.security.permissions.PermissionState;
import cs.system.security.permissions.FileIOPermission;
import cs.system.security.permissions.FileIOPermissionAccess;
import cs.system.AppDomain;


@:coreApi
class FileSystem {
	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(2 * cs.system.Environment.ProcessorCount);
	static final unixEpoch = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);

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

		The file will be automatically deleted when it is closed.

		Depending on a target platform the file may be automatically deleted upon
		application shutdown, but in general deletion is not guaranteed if the `close`
		method is not called.
	**/
	static public function tempFile(callback:Callback<File>):Void {
		throw new NotImplementedException();
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		pool.runFor(
			() -> {
				try Bytes.ofData(CsFile.ReadAllBytes(path))
				catch(e:CsException) rethrow(e, path);
			},
			callback
		);
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		pool.runFor(
			() -> {
				try CsFile.ReadAllText(path)
				catch(e:CsException) rethrow(e, path);
			},
			callback
		);
	}

	static public inline function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeNativeBytes(path, data.getData(), flag, callback);
	}

	static public inline function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		writeNativeBytes(path, UTF8.GetBytes(text), flag, callback);
	}

	static function writeNativeBytes(path:String, bytes:NativeArray<cs.StdTypes.UInt8>, flag:FileOpenFlag<Dynamic>, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				var stream = null;
				try {
					stream = streamFile(path, flag);
					stream.Write(bytes, 0, bytes.Length);
					stream.Close();
					NoData;
				} catch(e:CsException) {
					closeStream(stream);
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		pool.runFor(
			() -> {
				try {
					if(!CsDirectory.Exists(path))
						if(CsFile.Exists(path))
							throw new FsException(NotDirectory, path)
						else
							throw new FsException(FileNotFound, path);
					#if (net_ver >= 40)
						var contents = CsDirectory.EnumerateFileSystemEntries(path).GetEnumerator();
					#else
						var entries = CsDirectory.GetFileSystemEntries(path);
						var contents:Array<FilePath> = @:privateAccess Array.alloc(entries.length);
						for(i in 0...entries.length)
							contents[i] = FilePath.ofString(entries[i]);
					#end
					new Directory(contents, path, maxBatchSize);
				} catch(e:FsException) {
					throw e;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				try {
					var entries = CsDirectory.GetFileSystemEntries(path);
					var result:Array<FilePath> = @:privateAccess Array.alloc(entries.length);
					for(i in 0...entries.length)
						result[i] = FilePath.ofString(entries[i]).name();
					result;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(recursive)
						CsDirectory.CreateDirectory(path)
					else {
						switch path.parent() {
							case null:
							case parent if(!CsDirectory.Exists(parent)):
								throw new DirectoryNotFoundException(parent);
						}
						CsDirectory.CreateDirectory(path);
					}
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				if(prefix == null)
					prefix = '';
				var path = parentDirectory.add(prefix + Guid.NewGuid().ToString());
				try {
					if(recursive || CsDirectory.Exists(parentDirectory))
						CsDirectory.CreateDirectory(path)
					else
						throw new DirectoryNotFoundException(parentDirectory);
					path;
				} catch(e:DirectoryNotFoundException) {
					throw new FsException(FileNotFound, parentDirectory);
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					try {
						if(overwrite && CsFile.Exists(newPath))
							CsFile.Delete(newPath);
						CsFile.Move(oldPath, newPath);
					} catch(e:FileNotFoundException) {
						if(!overwrite && CsDirectory.Exists(newPath))
							throw new FsException(FileExists, newPath);
						CsDirectory.Move(oldPath, newPath);
					}
					NoData;
				} catch(e:FsException) {
					throw e;
				} catch(e:CsException) {
					rethrow(e, oldPath);
				}
			},
			callback
		);
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(!CsFile.Exists(path))
						throw new FileNotFoundException(path);
					CsFile.Delete(path);
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(!CsDirectory.Exists(path))
						throw new DirectoryNotFoundException(path);
					CsDirectory.Delete(path);
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					var fi = new CsFileInfo(path);
					if(!fi.Exists)
						throw new FileNotFoundException(path);
					({
						gid: 0,
						uid: 0,
						atime: Std.int(fi.LastAccessTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						mtime: Std.int(fi.LastWriteTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						ctime: Std.int(fi.CreationTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
						size: cast(fi.Length, Int),
						dev: 0,
						ino: 0,
						nlink: 0,
						rdev: 0,
						mode: @:privateAccess FileMode.S_IFREG,
						blksize: 0,
						blocks: 0
					}:FileInfo);
				} catch(e:FileNotFoundException) {
					try {
						var di = new DirectoryInfo(path);
						if(!di.Exists)
							throw new DirectoryNotFoundException(path);
						({
							gid: 0,
							uid: 0,
							atime: Std.int(di.LastAccessTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
							mtime: Std.int(di.LastWriteTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
							ctime: Std.int(di.CreationTime.ToUniversalTime().Subtract(unixEpoch).TotalSeconds),
							size: 0,
							dev: 0,
							ino: 0,
							nlink: 0,
							rdev: 0,
							mode: @:privateAccess FileMode.S_IFDIR,
							blksize: 0,
							blocks: 0
						}:FileInfo);
					} catch(e:CsException) {
						rethrow(e, path);
					}
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				var stream = null;
				try {
					var result = true;
					var isFile = CsFile.Exists(path);
					var isDir = !isFile && CsDirectory.Exists(path);
					if(mode.has(Exists) && !isFile && !isDir) {
						result = false;
					} else if(isFile) {
						stream = CsFile.Open(path, FileMode.Open);
						if(mode.has(Readable)) {
							result = result && stream.CanRead;
						}
						if(mode.has(Readable)) {
							result = result && stream.CanWrite;
						}
						if(mode.has(Executable)) {
							//TODO
						}
						stream.Close();
					} else if(isDir) { //if `isDir` is `true` it means the directory is at least readable, so check only for writable
						if(mode.has(Writable)) {
							var permissionSet = new PermissionSet(PermissionState.None);
							var writePermission = new FileIOPermission(FileIOPermissionAccess.Write, path.absolute());
							permissionSet.AddPermission(writePermission);
							if(!permissionSet.IsSubsetOf(untyped __cs__('System.AppDomain.CurrentDomain.PermissionSet'))) {
								result = false;
							}
						}
					} else {
						result = false;
					}
					result;
				} catch(e:CsException) {
					closeStream(stream);
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					CsDirectory.Exists(path);
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					CsFile.Exists(path);
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var attr = (cast CsFile.GetAttributes(path):Int);
					var ro = (cast FileAttributes.ReadOnly:Int);
					if(attr & 128 == 0) // u+w
						CsFile.SetAttributes(path, cast (attr | ro))
					else
						CsFile.SetAttributes(path, cast (attr & ~ro));
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
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
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		throw NotSupportedException.field();
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		throw NotSupportedException.field();
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					CsFile.Copy(source, destination, overwrite);
					NoData;
				} catch(e:CsException) {
					rethrow(e, source);
				}
			},
			callback
		);
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				var stream = null;
				try {
					stream = streamFile(path, OverwriteRead);
					stream.SetLength(newSize);
					stream.Close();
					NoData;
				} catch(e:CsException) {
					closeStream(stream);
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var epoch = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
					CsFile.SetLastAccessTimeUtc(path, epoch.AddSeconds(accessTime));
					CsFile.SetLastWriteTimeUtc(path, epoch.AddSeconds(modificationTime));
					NoData;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				try {
					//C# does not have API to resolve symlinks
					if(!CsFile.Exists(path))
						throw new FileNotFoundException('File not found', path);
					path.absolute().normalize();
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}

	static function streamFile(path:String, flag:FileOpenFlag<Dynamic>):FileStream {
		var mode = FileMode.Create;
		var access = FileAccess.ReadWrite;
		switch flag {
			case Append: mode = Append; access = Write;
			case Read: mode = Open; access = Read;
			case ReadWrite: mode = Open;
			case Write: mode = Create; access = Write;
			case WriteX: mode = CreateNew; access = Write;
			case WriteRead: mode = Create;
			case WriteReadX: mode = CreateNew;
			case Overwrite: mode = OpenOrCreate; access = Write;
			case OverwriteRead: mode = OpenOrCreate;
		}
		return new FileStream(path, mode, access, ReadWrite);
	}

	@:allow(asys.native.filesystem)
	static inline function rethrow<T>(e:CsException, path:FilePath):T {
		var error:IoErrorType = if(Std.isOfType(e, FileNotFoundException)) {
			FileNotFound;
		} else if(Std.isOfType(e, DirectoryNotFoundException)) {
			FileNotFound;
		} else {
			CustomError(e.Message);
		}
		throw new FsException(error, path);
	}

	static inline function closeStream(stream:Null<FileStream>):Void {
		if(stream != null)
			stream.Close();
	}
}
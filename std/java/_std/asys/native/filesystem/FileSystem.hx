package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.IJobExecutor;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import java.NativeArray;
import java.lang.Exception as JException;
import java.lang.Throwable;
import java.lang.Class as JClass;
import java.util.Set;
import java.io.RandomAccessFile;
import java.util.concurrent.TimeUnit;
import java.nio.file.Files;
import java.nio.file.Path as JPath;
import java.nio.file.StandardOpenOption;
import java.nio.file.OpenOption;
import java.nio.file.LinkOption;
import java.nio.file.StandardCopyOption;
import java.nio.file.CopyOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.FileSystemException;
import java.nio.file.NotDirectoryException;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.channels.FileChannel;

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

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void
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
					var channel = FileChannel.open(path, hxOpenFlagToJavaOption(flag));
					cast new File(path, channel, jobs);
				} catch(e:FileSystemException) {
					var reason = e.getReason();
					throw new FsException(CustomError(reason == null ? e.toString() : reason), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function tempFile(callback:Callback<File>):Void {
		jobs.addJob(
			() -> {
				try {
					var path = new FilePath(Files.createTempFile(@:nullSafety(Off) (null:String), @:nullSafety(Off) (null:String), new NativeArray(0)));
					var channel = FileChannel.open(path, hxOpenFlagToJavaOption(ReadWrite));
					cast new File(path, channel, jobs, true);
				} catch(e:FileSystemException) {
					var reason = e.getReason();
					throw new FsException(CustomError(reason == null ? e.toString() : reason), '(unknown path)');
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), '(unknown path)');
				}
			},
			callback
		);
	}

	public inline function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		jobs.addJob(
			() -> {
				try {
					Bytes.ofData(Files.readAllBytes(path));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function readString(path:FilePath, callback:Callback<String>):Void {
		jobs.addJob(
			() -> {
				try {
					var bytes = Files.readAllBytes(path);
					new String(bytes, 0, bytes.length, "UTF-8");
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.write(path, data.getData(), hxOpenFlagToJavaOption(flag));
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.write(path, @:privateAccess text.getBytes("UTF-8"), hxOpenFlagToJavaOption(flag));
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		jobs.addJob(
			() -> {
				try {
					new Directory(path, Files.newDirectoryStream(path), jobs);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		jobs.addJob(
			() -> {
				try {
					var result = [];
					var dir = Files.newDirectoryStream(path);
					for(entry in dir) {
						result.push(new FilePath(entry.getFileName()));
					}
					result;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		var jPerm:Null<Set<PosixFilePermission>> = permissions;
		var jPerm:Set<PosixFilePermission> = jPerm == null ? FilePermissions.octal(0, 7, 7, 7) : jPerm;
		jobs.addJob(
			() -> {
				try {
					var attributes = NativeArray.make(PosixFilePermissions.asFileAttribute(jPerm));
					if(recursive)
						Files.createDirectories(path, attributes)
					else
						Files.createDirectory(path, attributes);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		var prefix:String = prefix == null ? '' : prefix;
		var jPerm:Null<Set<PosixFilePermission>> = permissions;
		var jPerm:Set<PosixFilePermission> = jPerm == null ? FilePermissions.octal(0, 7, 7, 7) : jPerm;
		jobs.addJob(
			() -> {
				try {
					var attributes = NativeArray.make(PosixFilePermissions.asFileAttribute(jPerm));
					if(recursive)
						Files.createDirectories(parentDirectory, attributes);
					Files.createTempDirectory(parentDirectory, prefix, attributes);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), parentDirectory);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), parentDirectory);
				}
			},
			callback
		);
	}

	public inline function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var options = overwrite ? NativeArray.make((cast StandardCopyOption.REPLACE_EXISTING:CopyOption)) : new NativeArray(0);
					Files.move(oldPath, newPath, options);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), oldPath);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), oldPath);
				}
			},
			callback
		);
	}

	public inline function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(Files.isDirectory(path, new NativeArray(0))) {
						throw new JException('Not a file');
					}
					Files.delete(path);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					if(Files.isDirectory(path, new NativeArray(0))) {
						Files.delete(path);
					} else {
						throw new NotDirectoryException(path);
					}
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function info(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.readAttributes(path, javaClass(PosixFileAttributes), new NativeArray(0));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					(!mode.has(Exists) || Files.exists(path, new NativeArray(0)))
					&& (!mode.has(Readable) || Files.isReadable(path))
					&& (!mode.has(Writable) || Files.isWritable(path))
					&& (!mode.has(Executable) || Files.isExecutable(path));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.isDirectory(path, new NativeArray(0));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function isFile(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.isRegularFile(path, new NativeArray(0));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.setPosixFilePermissions(path, permissions);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView), new NativeArray(0));
					attributes.setOwner(user);
					attributes.setGroup(group);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView), NativeArray.make(NOFOLLOW_LINKS));
					attributes.setOwner(user);
					attributes.setGroup(group);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					switch type {
						case HardLink: Files.createLink(path, target);
						case SymLink: Files.createSymbolicLink(path, target, new NativeArray(0));
					}
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function isLink(path:FilePath, callback:Callback<Bool>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.isSymbolicLink(path);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					new FilePath(Files.readSymbolicLink(path));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		jobs.addJob(
			() -> {
				try {
					Files.readAttributes(path, javaClass(PosixFileAttributes), NativeArray.make(NOFOLLOW_LINKS));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var options = overwrite
						? NativeArray.make((cast NOFOLLOW_LINKS:CopyOption), (cast REPLACE_EXISTING:CopyOption))
						: NativeArray.make((cast NOFOLLOW_LINKS:CopyOption));
					Files.copy(source, destination, options);
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), source);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), source);
				}
			},
			callback
		);
	}

	public inline function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var f = new RandomAccessFile((path:JPath).toFile(), 'rw');
					f.setLength(newSize);
					f.close();
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		jobs.addJob(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView), new NativeArray(0));
					attributes.setTimes(FileTime.from(modificationTime, SECONDS), FileTime.from(accessTime, SECONDS), @:nullSafety(Off) (null:FileTime));
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public inline function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		jobs.addJob(
			() -> {
				try {
					new FilePath((path:JPath).toRealPath(new NativeArray(0)));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static function hxOpenFlagToJavaOption(flag:FileOpenFlag<Dynamic>):NativeArray<OpenOption> {
		return switch flag {
			case Append: cast NativeArray.make(CREATE, WRITE, APPEND);
			case Read: cast NativeArray.make(READ);
			case ReadWrite: cast NativeArray.make(READ, WRITE);
			case Write: cast NativeArray.make(CREATE, WRITE, TRUNCATE_EXISTING);
			case WriteX: cast NativeArray.make(CREATE_NEW, WRITE);
			case WriteRead: cast NativeArray.make(CREATE, WRITE, READ, TRUNCATE_EXISTING);
			case WriteReadX: cast NativeArray.make(CREATE_NEW, WRITE, READ);
			case Overwrite: cast NativeArray.make(CREATE, WRITE);
			case OverwriteRead: cast NativeArray.make(CREATE, WRITE, READ);
		}
	}

	static inline function javaClass<T>(cls:Class<T>):JClass<T> {
		return cast cls;
	}
}
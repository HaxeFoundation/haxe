package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import sys.thread.ElasticThreadPool;
import java.NativeArray;
import java.lang.Exception as JException;
import java.lang.Throwable;
import java.lang.Class as JClass;
import java.lang.Runtime;
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
	@:allow(asys.native.filesystem)
	static final pool = new ElasticThreadPool(2 * Runtime.getRuntime().availableProcessors());

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		inline pool.runFor(
			() -> {
				try {
					var channel = FileChannel.open(path, ...hxOpenFlagToJavaOption(flag));
					cast new File(path, channel);
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

	static public function tempFile(callback:Callback<File>):Void {
		pool.runFor(
			() -> {
				try {
					var path = new FilePath(Files.createTempFile(@:nullSafety(Off) (null:String), @:nullSafety(Off) (null:String)));
					var channel = FileChannel.open(path, ...hxOpenFlagToJavaOption(ReadWrite));
					cast new File(path, channel, true);
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

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		pool.runFor(
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

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		pool.runFor(
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

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Files.write(path, data.getData(), ...hxOpenFlagToJavaOption(flag));
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

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Files.write(path, @:privateAccess text.getBytes("UTF-8"), ...hxOpenFlagToJavaOption(flag));
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

	static public function openDirectory(path:FilePath, maxBatchSize:Int = 64, callback:Callback<Directory>):Void {
		pool.runFor(
			() -> {
				try {
					new Directory(path, Files.newDirectoryStream(path), maxBatchSize);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
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

	static public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void {
		var jPerm:Null<Set<PosixFilePermission>> = permissions;
		var jPerm:Set<PosixFilePermission> = jPerm == null ? FilePermissions.octal(0, 7, 7, 7) : jPerm;
		pool.runFor(
			() -> {
				try {
					var attributes = PosixFilePermissions.asFileAttribute(jPerm);
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

	static public function uniqueDirectory(parentDirectory:FilePath, ?prefix:String, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void {
		var prefix:String = prefix == null ? '' : prefix;
		var jPerm:Null<Set<PosixFilePermission>> = permissions;
		var jPerm:Set<PosixFilePermission> = jPerm == null ? FilePermissions.octal(0, 7, 7, 7) : jPerm;
		pool.runFor(
			() -> {
				try {
					var attributes = PosixFilePermissions.asFileAttribute(jPerm);
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

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var options = overwrite ? NativeArray.make((cast StandardCopyOption.REPLACE_EXISTING:CopyOption)) : new NativeArray(0);
					Files.move(oldPath, newPath, ...options);
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

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(Files.isDirectory(path)) {
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

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(Files.isDirectory(path)) {
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

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					Files.readAttributes(path, javaClass(PosixFileAttributes));
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
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
					(!mode.has(Exists) || Files.exists(path))
					&& (!mode.has(Readable) || Files.isReadable(path))
					&& (!mode.has(Writable) || Files.isWritable(path))
					&& (!mode.has(Executable) || Files.isExecutable(path));
				} catch(e:Throwable) {
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
					Files.isDirectory(path);
				} catch(e:Throwable) {
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
					Files.isRegularFile(path);
				} catch(e:Throwable) {
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

	static public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView));
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

	static public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView), NOFOLLOW_LINKS);
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

	static public function link(target:FilePath, path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					switch type {
						case HardLink: Files.createLink(path, target);
						case SymLink: Files.createSymbolicLink(path, target);
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

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		pool.runFor(
			() -> {
				try {
					Files.isSymbolicLink(path);
				} catch(e:Throwable) {
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

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					Files.readAttributes(path, javaClass(PosixFileAttributes), NOFOLLOW_LINKS);
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var options = overwrite
						? NativeArray.make((cast NOFOLLOW_LINKS:CopyOption), (cast REPLACE_EXISTING:CopyOption))
						: NativeArray.make((cast NOFOLLOW_LINKS:CopyOption));
					Files.copy(source, destination, ...options);
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

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		pool.runFor(
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

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					var attributes = Files.getFileAttributeView(path, javaClass(PosixFileAttributeView));
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

	static public function realPath(path:FilePath, callback:Callback<FilePath>):Void {
		pool.runFor(
			() -> {
				try {
					new FilePath((path:JPath).toRealPath());
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
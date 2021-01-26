package eval.luv;

import eval.integers.Int64;
import eval.integers.UInt64;

@:forward
abstract FileRequest(Request) to Request {}

enum abstract FileOpenFlag(Int) {
	var RDONLY = 0;
	var WRONLY = 1;
	var RDWR = 2;
	var CREAT = 3;
	var EXCL = 4;
	var EXLOCK = 5;
	var NOCTTY = 6;
	var NOFOLLOW = 7;
	var TEMPORARY = 8;
	var TRUNC = 9;
	var APPEND = 10;
	var DIRECT = 11;
	var DSYNC = 12;
	var FILEMAP = 13;
	var NOATIME = 14;
	var NONBLOCK = 15;
	var RANDOM = 16;
	var SEQUENTIAL = 17;
	var SHORT_LIVED = 18;
	var SYMLINK = 19;
	var SYNC = 20;
}

/**
	Permission bits.
	@see https://aantron.github.io/luv/luv/Luv/File/Mode
**/
enum FileMode {
	IRWXU;
	IRUSR;
	IWUSR;
	IXUSR;
	IRWXG;
	IRGRP;
	IWGRP;
	IXGRP;
	IRWXO;
	IROTH;
	IWOTH;
	IXOTH;
	ISUID;
	ISGID;
	ISVTX;
	IFMT;
	IFREG;
	IFDIR;
	IFBLK;
	IFCHR;
	IFLNK;
	IFIFO;
	NUMERIC(mode:Int);
}

/**
	Abstract type for a bit field of permissions bits, i.e., an `int` in which
	multiple bits may be set. These bit fields are returned by operations such
	as `eval.luv.File.stat`
**/
@:coreType abstract FileModeNumeric {}

typedef FileStatTimeSpec = {
	var sec:Int64;
	var nsec:Int64;
}

typedef FileStat = {
	var dev:UInt64;
	var mode:FileModeNumeric;
	var nlink:UInt64;
	var uid:UInt64;
	var gid:UInt64;
	var rdev:UInt64;
	var ino:UInt64;
	var size:UInt64;
	var blksize:UInt64;
	var blocks:UInt64;
	var flags:UInt64;
	var gen:UInt64;
	var atim:FileStatTimeSpec;
	var mtim:FileStatTimeSpec;
	var ctim:FileStatTimeSpec;
	var birthtim:FileStatTimeSpec;
}

typedef FileStatFs = {
	var type:UInt64;
	var bsize:UInt64;
	var blocks:UInt64;
	var bfree:UInt64;
	var bavail:UInt64;
	var files:UInt64;
	var ffree:UInt64;
	var fspare:Array<UInt64>;
}

enum abstract FileCopyFlag(Int) {
	var COPYFILE_EXCL = 0;
	var COPYFILE_FICLONE = 1;
	var COPYFILE_FICLONE_FORCE = 2;
}

enum abstract FileAccessFlag(Int) {
	var F_OK = 0;
	var R_OK = 1;
	var W_OK = 2;
	var X_OK = 3;
}

enum abstract FileSymlinkFlag(Int) {
	var SYMLINK_DIR = 0;
	var SYMLINK_JUNCTION = 1;
}

/**
	Files.

	@see https://aantron.github.io/luv/luv/Luv/File
**/
@:using(eval.luv.Handle)
@:coreType abstract File to Handle {

	extern static public final stdin:File;
	extern static public final stdout:File;
	extern static public final stderr:File;

	static public function createRequest():FileRequest;

	/**
		Checks whether all the bits in `mask` are set in `bits`.

		For example, if `bits` is equal to octal 0o644, then
		`eval.luv.File.testMode [IRUSR] bits` evaluates to `true`.
	**/
	static public function testMode(mask:Array<FileMode>, bits:FileModeNumeric):Bool;

	/**
		Opens the file at the given path.
		The default value of the `mode` argument is equal to octal `0o644`.
	**/
	static public function open(loop:Loop, path:NativeString, flags:Array<FileOpenFlag>, ?mode:Array<FileMode>, ?request:FileRequest, callback:(result:Result<File>)->Void):Void;

	/**
		Closes the file.
	**/
	public function close(loop:Loop, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Reads from the file.

		The incoming data is written consecutively to into the given buffers.
		The number of bytes that the operation tries to read is the total length
		of the buffers.

		End of file is indicated by `Result.Ok(0)`. Note that this is different
		from `eval.luv.Stream.readStart`.
	**/
	public function read(loop:Loop, fileOffset:Int64, buffers:Array<Buffer>, ?request:FileRequest, callback:(result:Result<UInt64>)->Void):Void;

	/**
		Writes to the file.
	**/
	public function write(loop:Loop, fileOffset:Int64, buffers:Array<Buffer>, ?request:FileRequest, callback:(result:Result<UInt64>)->Void):Void;

	/**
		Deletes the file at the given path.
	**/
	static public function unlink(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Moves the file at the given path to the path given by `toPath`
	**/
	static public function rename(loop:Loop, path:NativeString, toPath:NativeString, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Creates a temporary file with name based on the given pattern.
	**/
	static public function mkstemp(loop:Loop, pattern:NativeString, ?request:FileRequest, callback:(result:Result<{name:NativeString,file:File}>)->Void):Void;

	/**
		Creates a temporary directory with name based on the given pattern.
	**/
	static public function mkdtemp(loop:Loop, pattern:NativeString, ?request:FileRequest, callback:(result:Result<NativeString>)->Void):Void;

	/**
		Creates a directory.
	**/
	static public function mkdir(loop:Loop, path:NativeString, ?mode:Array<FileMode>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Deletes a directory.
	**/
	static public function rmdir(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Retrieves status information for the file at the given path.
	**/
	static public function stat(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<FileStat>)->Void):Void;

	/**
		Like `eval.luv.File.stat`, but does not dereference symlinks.
	**/
	static public function lstat(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<FileStat>)->Void):Void;

	/**
		Retrieves status information for this file.
	**/
	public function fstat(loop:Loop, ?request:FileRequest, callback:(result:Result<FileStat>)->Void):Void;

	/**
		Retrieves status information for the filesystem containing the given path.
	**/
	static public function statFs(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<FileStatFs>)->Void):Void;

	/**
		Flushes file changes to storage.
	**/
	public function fsync(loop:Loop, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Like `eval.luv.File.fsync`, but may omit some metadata.
	**/
	public function fdataSync(loop:Loop, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Truncates the given file to the given length.
	**/
	public function ftruncate(loop:Loop, length:Int64, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Copies the file at the given path to the path given by `toPath`.
	**/
	static public function copyFile(loop:Loop, path:NativeString, toPath:NativeString, ?flags:Array<FileCopyFlag>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Transfers data between file descriptors.
	**/
	public function sendFile(loop:Loop, toFile:File, offset:Int64, length:UInt64, ?request:FileRequest, callback:(result:Result<UInt64>)->Void):Void;

	/**
		Checks whether the calling process can access the file at the given path.
	**/
	static public function access(loop:Loop, path:NativeString, flags:Array<FileAccessFlag>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Changes permissions of the file at the given path.
	**/
	static public function chmod(loop:Loop, path:NativeString, mode:Array<FileMode>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Changes permissions of the file.
	**/
	public function fchmod(loop:Loop, mode:Array<FileMode>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Sets timestamps of the file at the given path.
	**/
	static public function utime(loop:Loop, path:NativeString, atime:Float, mtime:Float, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Sets timestamps of the file.
	**/
	public function futime(loop:Loop, atime:Float, mtime:Float, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Sets timestamps of the file at the given path without dereferencing symlinks.
	**/
	static public function lutime(loop:Loop, path:NativeString, atime:Float, mtime:Float, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Hardlinks a file at the location given by `link`.
	**/
	static public function link(loop:Loop, path:NativeString, link:NativeString, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Symlinks a file at the location given by `link`.
	**/
	static public function symlink(loop:Loop, path:NativeString, link:NativeString, ?flags:Array<FileSymlinkFlag>, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Reads the target path of a symlink.
	**/
	static public function readLink(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<NativeString>)->Void):Void;

	/**
		Resolves a real absolute path to the given file.
	**/
	static public function realPath(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<NativeString>)->Void):Void;

	/**
		Changes owneship of the file at the given path.
	**/
	static public function chown(loop:Loop, path:NativeString, uid:Int, gid:Int, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Changes owneship of the file at the given path. without dereferencing symlinks.
	**/
	static public function lchown(loop:Loop, path:NativeString, uid:Int, gid:Int, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Changes owneship of the file.
	**/
	public function fchown(loop:Loop, uid:Int, gid:Int, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Returns the integer representation of `eval.luv.File`.

		`eval.luv.File` is defined as an integer file descriptor by libuv on all
		platforms at the moment. This is a convenience function for interoperability
		with `eval.luv.Process`, the API of which assumes that files are represented
		by integers.
	**/
	public function toInt():Int;
}

/**
	Synchronous version of `eval.luv.File` API
**/
extern class FileSync {
	@:inheritDoc(eval.luv.File.open)
	static function open(path:NativeString, flags:Array<FileOpenFlag>, ?mode:Array<FileMode>):Result<File>;

	@:inheritDoc(eval.luv.File.close)
	static function close(file:File):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.read)
	static function read(file:File, fileOffset:Int64, buffers:Array<Buffer>):Result<UInt64>;

	@:inheritDoc(eval.luv.File.write)
	static function write(file:File, fileOffset:Int64, buffers:Array<Buffer>):Result<UInt64>;

	@:inheritDoc(eval.luv.File.unlink)
	static function unlink(path:NativeString):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.rename)
	static function rename(path:NativeString, toPath:NativeString):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.mkstemp)
	static function mkstemp(pattern:NativeString):Result<{name:NativeString,file:File}>;

	@:inheritDoc(eval.luv.File.mkdtemp)
	static function mkdtemp(pattern:NativeString):Result<NativeString>;

	@:inheritDoc(eval.luv.File.mkdir)
	static function mkdir(path:NativeString, ?mode:Array<FileMode>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.rmdir)
	static function rmdir(path:NativeString):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.stat)
	static function stat(path:NativeString):Result<FileStat>;

	@:inheritDoc(eval.luv.File.lstat)
	static function lstat(path:NativeString):Result<FileStat>;

	@:inheritDoc(eval.luv.File.fstat)
	static function fstat(file:File):Result<FileStat>;

	@:inheritDoc(eval.luv.File.statFs)
	static function statFs(path:NativeString):Result<FileStatFs>;

	@:inheritDoc(eval.luv.File.fsync)
	static function fsync(file:File):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.fdataSync)
	static function fdataSync(file:File):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.ftruncate)
	static function ftruncate(file:File, length:Int64):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.copyFile)
	static function copyFile(path:NativeString, toPath:NativeString, ?flags:Array<FileCopyFlag>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.sendFile)
	static function sendFile(file:File, toFile:File, offset:Int64, length:UInt64):Result<UInt64>;

	@:inheritDoc(eval.luv.File.access)
	static function access(path:NativeString, flags:Array<FileAccessFlag>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.chmod)
	static function chmod(path:NativeString, mode:Array<FileMode>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.fchmod)
	static function fchmod(file:File, mode:Array<FileMode>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.utime)
	static function utime(path:NativeString, atime:Float, mtime:Float):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.futime)
	static function futime(file:File, atime:Float, mtime:Float):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.lutime)
	static function lutime(path:NativeString, atime:Float, mtime:Float):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.link)
	static function link(path:NativeString, link:NativeString):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.symlink)
	static function symlink(path:NativeString, link:NativeString, ?flags:Array<FileSymlinkFlag>):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.readLink)
	static function readLink(path:NativeString):Result<NativeString>;

	@:inheritDoc(eval.luv.File.realPath)
	static function realPath(path:NativeString):Result<NativeString>;

	@:inheritDoc(eval.luv.File.chown)
	static function chown(path:NativeString, uid:Int, gid:Int):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.lchown)
	static function lchown(path:NativeString, uid:Int, gid:Int):Result<Result.NoData>;

	@:inheritDoc(eval.luv.File.fchown)
	static function fchown(file:File, uid:Int, gid:Int):Result<Result.NoData>;

}
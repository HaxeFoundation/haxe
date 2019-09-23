package asys;

import haxe.Error;
import haxe.io.Bytes;
import haxe.io.FilePath;
import asys.io.*;

typedef FileReadStreamCreationOptions = {
	?flags:FileOpenFlags,
	?mode:FilePermissions
} &
	asys.io.FileReadStream.FileReadStreamOptions;

/**
	This class provides methods for synchronous operations on files and
	directories. For asynchronous operations, see `asys.async.FileSystem`.

	Passing `null` as a path to any of the functions in this class will result
	in unspecified behaviour.
**/
extern class FileSystem {
	public static inline final async = asys.AsyncFileSystem;

	/**
		Tests specific user permissions for the file specified by `path`. If the
		check fails, throws an exception. `mode` is one or more `FileAccessMode`
		values:

		- `FileAccessMode.Ok` - file is visible to the calling process (it exists)
		- `FileAccessMode.Execute` - file can be executed by the calling process
		- `FileAccessMode.Write` - file can be written to by the calling process
		- `FileAccessMode.Read` - file can be read from by the calling process

		Mode values can be combined with the bitwise or operator, e.g. calling
		`access` with the `mode`:

		```haxe
		FileAccessMode.Execute | FileAccessMode.Read
		```

		will check that the file is both readable and executable.

		The result of this call should not be used in a condition before a call to
		e.g. `open`, because this would introduce a race condition (the file could
		be deleted after the `access` call, but before the `open` call). Instead,
		the latter function should be called immediately and errors should be
		handled with a `try ... catch` block.
	**/
	static function access(path:FilePath, ?mode:FileAccessMode = FileAccessMode.Ok):Void;

	/**
		Appends `data` at the end of the file located at `path`.
	**/
	static function appendFile(path:FilePath, data:Bytes, ?flags:FileOpenFlags /* a */, ?mode:FilePermissions /* 0666 */):Void;

	/**
		Changes the permissions of the file specific by `path` to `mode`.

		If `path` points to a symbolic link, this function will change the
		permissions of the target file, not the symbolic link itself, unless
		`followSymLinks` is set to `false`.

		TODO: `followSymLinks == false` is not implemented and will throw.
	**/
	static function chmod(path:FilePath, mode:FilePermissions, ?followSymLinks:Bool = true):Void;

	/**
		Changes the owner and group of the file specific by `path` to `uid` and
		`gid`, respectively.

		If `path` points to a symbolic link, this function will change the
		permissions of the target file, not the symbolic link itself, unless
		`followSymLinks` is set to `false`.

		TODO: `followSymLinks == false` is not implemented and will throw.
	**/
	static function chown(path:FilePath, uid:Int, gid:Int, ?followSymLinks:Bool = true):Void;

	/**
		Copies the file at `src` to `dest`. If `dest` exists, it is overwritten.
	**/
	static function copyFile(src:FilePath, dest:FilePath /* , ?flags:FileCopyFlags */):Void;

	/**
		Creates a read stream (an instance of `IReadable`) for the given path.
		`options` can be used to specify how the file is opened, as well as which
		part of the file will be read by the stream.

		- `options.flags` - see `open`.
		- `options.mode` - see `open`.
		- `options.autoClose` - whether the file should be closed automatically
			once the stream is fully consumed.
		- `options.start` - starting position in bytes (inclusive).
		- `options.end` - end position in bytes (non-inclusive).
	**/
	static function createReadStream(path:FilePath, ?options:FileReadStreamCreationOptions):FileReadStream;

	// static function createWriteStream(path:FilePath, ?options:{?flags:FileOpenFlags, ?mode:FilePermissions, ?autoClose:Bool, ?start:Int}):FileWriteStream;

	/**
		Returns `true` if the file or directory specified by `path` exists.

		The result of this call should not be used in a condition before a call to
		e.g. `open`, because this would introduce a race condition (the file could
		be deleted after the `exists` call, but before the `open` call). Instead,
		the latter function should be called immediately and errors should be
		handled with a `try ... catch` block.
	**/
	static function exists(path:FilePath):Bool;

	static function link(existingPath:FilePath, newPath:FilePath):Void;

	/**
		Creates a directory at the path `path`, with file mode `mode`.

		If `recursive` is `false` (default), this function can only create one
		directory at a time, the last component of `path`. If `recursive` is `true`,
		intermediate directories will be created as needed.
	**/
	static function mkdir(path:FilePath, ?recursive:Bool = false, ?mode:FilePermissions /* 0777 */):Void;

	/**
		Creates a unique temporary directory. `prefix` should be a path template
		ending in six `X` characters, which will be replaced with random characters.
		Returns the path to the created directory.

		The generated directory needs to be manually deleted by the process.
	**/
	static function mkdtemp(prefix:FilePath):FilePath;

	/**
		Opens the file located at `path`.
	**/
	static function open(path:FilePath, ?flags:FileOpenFlags /* a */, ?mode:FilePermissions /* 0666 */, ?binary:Bool = true):File;

	/**
		Reads the contents of a directory specified by `path`. Returns an array of
		`FilePath`s relative to the specified directory (i.e. the paths are not
		absolute). The array will not include `.` or `..`.
	**/
	static function readdir(path:FilePath):Array<FilePath>;

	/**
		Same as `readdir`, but returns an array of `DirectoryEntry` values instead.
	**/
	static function readdirTypes(path:FilePath):Array<DirectoryEntry>;

	/**
		Reads all the bytes of the file located at `path`.
	**/
	static function readFile(path:FilePath, ?flags:FileOpenFlags /* r */):Bytes;

	/**
		Returns the contents (target path) of the symbolic link located at `path`.
	**/
	static function readlink(path:FilePath):FilePath;

	/**
		Returns the canonical path name of `path` (which may be a relative path)
		by resolving `.`, `..`, and symbolic links.
	**/
	static function realpath(path:FilePath):FilePath;

	/**
		Renames the file or directory located at `oldPath` to `newPath`. If a file
		already exists at `newPath`, it is overwritten. If a directory already
		exists at `newPath`, an exception is thrown.
	**/
	static function rename(oldPath:FilePath, newPath:FilePath):Void;

	/**
		Deletes the directory located at `path`. If the directory is not empty or
		cannot be deleted, an error is thrown.
	**/
	static function rmdir(path:FilePath):Void;

	/**
		Returns information about the file located at `path`.

		If `path` points to a symbolic link, this function will return information
		about the target file, not the symbolic link itself, unless `followSymLinks`
		is set to `false`.
	**/
	static function stat(path:FilePath, ?followSymLinks:Bool = true):asys.FileStat;

	/**
		Creates a symbolic link at `path`, pointing to `target`.

		The `type` argument is ignored on all platforms except `Windows`.
	**/
	static function symlink(target:FilePath, path:FilePath, ?type:SymlinkType = SymlinkType.SymlinkDir):Void;

	/**
		Truncates the file located at `path` to exactly `len` bytes. If the file was
		larger than `len` bytes, the extra data is lost. If the file was smaller
		than `len` bytes, the file is extended with null bytes.
	**/
	static function truncate(path:FilePath, ?len:Int = 0):Void;

	/**
		Deletes the file located at `path`.
	**/
	static function unlink(path:FilePath):Void;

	/**
		Modifies the system timestamps of the file located at `path`.
	**/
	static function utimes(path:FilePath, atime:Date, mtime:Date):Void;

	/**
		Creates a file watcher for `path`.

		@param recursive If `true`, the file watcher will signal for changes in
			sub-directories of `path` as well.
	**/
	static function watch(path:FilePath, ?recursive:Bool = false):FileWatcher;

	/**
		Writes `data` to the file located at `path`.
	**/
	static function writeFile(path:FilePath, data:Bytes, ?flags:FileOpenFlags /* w */, ?mode:FilePermissions /* 0666 */):Void;
}

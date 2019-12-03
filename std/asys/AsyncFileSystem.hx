package asys;

import haxe.NoData;
import haxe.async.Callback;
import haxe.io.Bytes;
import haxe.io.FilePath;
import asys.*;

/**
	This class provides methods for asynchronous operations on files and
	directories. For synchronous operations, see `asys.FileSystem`.

	All methods here are asynchronous versions of the functions in
	`asys.FileSystem`. Please see them for a description of the arguments and
	use of each method.

	Any synchronous method that returns no value (`Void` return type) has an
	extra `callback:Callback<NoData>` argument.

	Any synchronous method that returns a value has an extra
	`callback:Callback<T>` argument, where `T` is the return type of the
	synchronous method.

	Errors are communicated through the callbacks or in some cases thrown
	immediately.
**/
extern class AsyncFileSystem {
	static function access(path:FilePath, ?mode:FileAccessMode = FileAccessMode.Ok, callback:Callback<NoData>):Void;
	static function chmod(path:FilePath, mode:FilePermissions, ?followSymLinks:Bool = true, callback:Callback<NoData>):Void;
	static function chown(path:FilePath, uid:Int, gid:Int, ?followSymLinks:Bool = true, callback:Callback<NoData>):Void;
	static function copyFile(src:FilePath, dest:FilePath, ?flags:FileCopyFlags, callback:Callback<NoData>):Void;
	static function exists(path:FilePath, callback:Callback<Bool>):Void;
	static function link(existingPath:FilePath, newPath:FilePath, callback:Callback<NoData>):Void;
	static function mkdir(path:FilePath, ?recursive:Bool, ?mode:FilePermissions, callback:Callback<NoData>):Void;
	static function mkdtemp(prefix:FilePath, callback:Callback<FilePath>):Void;
	static function readdir(path:FilePath, callback:Callback<Array<FilePath>>):Void;
	static function readdirTypes(path:FilePath, callback:Callback<Array<DirectoryEntry>>):Void;
	static function readlink(path:FilePath, callback:Callback<FilePath>):Void;
	static function realpath(path:FilePath, callback:Callback<FilePath>):Void;
	static function rename(oldPath:FilePath, newPath:FilePath, callback:Callback<NoData>):Void;
	static function rmdir(path:FilePath, callback:Callback<NoData>):Void;
	static function stat(path:FilePath, ?followSymLinks:Bool = true, callback:Callback<FileStat>):Void;
	static function symlink(target:FilePath, path:FilePath, ?type:String, callback:Callback<NoData>):Void;
	static function truncate(path:FilePath, len:Int, callback:Callback<NoData>):Void;
	static function unlink(path:FilePath, callback:Callback<NoData>):Void;
	static function utimes(path:FilePath, atime:Date, mtime:Date, callback:Callback<NoData>):Void;
	static function appendFile(path:FilePath, data:Bytes, ?flags:FileOpenFlags, ?mode:FilePermissions, callback:Callback<NoData>):Void;
	static function open(path:FilePath, ?flags:FileOpenFlags, ?mode:FilePermissions, ?binary:Bool = true, callback:Callback<sys.io.File>):Void;
	static function readFile(path:FilePath, ?flags:FileOpenFlags, callback:Callback<Bytes>):Void;
	static function writeFile(path:FilePath, data:Bytes, ?flags:FileOpenFlags, ?mode:FilePermissions, callback:Callback<NoData>):Void;
}

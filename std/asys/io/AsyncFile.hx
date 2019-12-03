package asys.io;

import haxe.NoData;
import haxe.async.*;
import haxe.io.Bytes;
import haxe.io.Encoding;
import asys.*;

/**
	This class provides methods for asynchronous operations on files instances.
	For synchronous operations, see `asys.io.File`. To obtain an instance of
	this class, use the `async` field of `asys.io.File`.

	```haxe
	var file = asys.FileSystem.open("example.txt", "r");
	file.async.readFile(contents -> trace(contents.toString()));
	```

	All methods here are asynchronous versions of the functions in
	`asys.io.File`. Please see them for a description of the arguments and
	use of each method.

	Any synchronous method that returns no value (`Void` return type) has an
	extra `callback:Callback<NoData>` argument.

	Any synchronous method that returns a value has an extra
	`callback:Callback<T>` argument, where `T` is the return type of the
	synchronous method.

	Errors are communicated through the callbacks or in some cases thrown
	immediately.
**/
extern class AsyncFile {
	function chmod(mode:FilePermissions, callback:Callback<NoData>):Void;
	function chown(uid:Int, gid:Int, callback:Callback<NoData>):Void;
	function close(callback:Callback<NoData>):Void;
	function datasync(callback:Callback<NoData>):Void;
	function readBuffer(buffer:Bytes, offset:Int, length:Int, position:Int, callback:Callback<{bytesRead:Int, buffer:Bytes}>):Void;
	function readFile(callback:Callback<Bytes>):Void;
	function stat(callback:Callback<FileStat>):Void;
	function sync(callback:Callback<NoData>):Void;
	function truncate(?len:Int = 0, callback:Callback<NoData>):Void;
	function utimes(atime:Date, mtime:Date, callback:Callback<NoData>):Void;
	function writeBuffer(buffer:Bytes, offset:Int, length:Int, position:Int, callback:Callback<{bytesWritten:Int, buffer:Bytes}>):Void;
	function writeString(str:String, ?position:Int, ?encoding:Encoding, callback:Callback<{bytesWritten:Int, buffer:Bytes}>):Void;
}

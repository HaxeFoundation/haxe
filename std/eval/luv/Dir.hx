package eval.luv;

import eval.luv.File;

enum abstract DirentKind(Int) {
	var UNKNOWN = 0;
	var FILE = 1;
	var DIR = 2;
	var LINK = 3;
	var FIFO = 4;
	var SOCKET = 5;
	var CHAR = 6;
	var BLOCK = 7;
}

typedef Dirent = {
	var kind:DirentKind;
	var name:NativeString;
}

typedef DirectoryScan = {
	/**
		Retrieves the next directory entry.
	**/
	function next():Null<Dirent>;

	/**
		Cleans up after a directory scan.
	**/
	function end():Void;
}

/**
	@see https://aantron.github.io/luv/luv/Luv/File#module-Dir
**/
@:coreType abstract Dir {
	/**
		Opens the directory at the given path for listing.
	**/
	static public function open(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<Dir>)->Void):Void;

	/**
		Closes the directory.
	**/
	public function close(loop:Loop, ?request:FileRequest, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Retrieves a directory entry.
	**/
	public function read(loop:Loop, ?numberOfEntries:Int, ?request:FileRequest, callback:(result:Result<Array<Dirent>>)->Void):Void;

	/**
		Begins directory listing.
	**/
	static public function scan(loop:Loop, path:NativeString, ?request:FileRequest, callback:(result:Result<DirectoryScan>)->Void):Void;
}

/**
	Synchronous version of `eval.luv.Dir` API
**/
extern class DirSync {
	@:inheritDoc(eval.luv.Dir.open)
	static public function open(path:NativeString):Result<Dir>;

	@:inheritDoc(eval.luv.Dir.close)
	static public function close(dir:Dir):Result<Result.NoData>;

	@:inheritDoc(eval.luv.Dir.read)
	static public function read(dir:Dir, ?numberOfEntries:Int):Result<Array<Dirent>>;

	@:inheritDoc(eval.luv.Dir.scan)
	static public function scan(path:NativeString):Result<DirectoryScan>;
}
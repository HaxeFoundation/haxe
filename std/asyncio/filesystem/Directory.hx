package asyncio.filesystem;

import haxe.NoData;
import haxe.errors.NotImplemented;
import haxe.Callback;

/**
	Represents a directory.
**/
class Directory {
	/**
		Path to this directory.
	**/
	public final path:FilePath;
	/**
		How many entries are buffered internally when reading from the directory.
		Higher numbers improve performance, but increase memory usage.
	**/
	public var buffer:Int = 32;

	//TODO: this is a dummy constructor to make the compiler shut up about uninitialized finals.
	function new() {
		path = cast null;
	}

	/**
		Read next directory entry.
	**/
	public function read(callback:Callback<Null<FilePath>>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Close the directory.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}
package asys.native.filesystem;

import haxe.NoData;
import haxe.exceptions.NotImplementedException;

/**
	Represents a directory.
**/
@:coreApi
class Directory {
	/** The path of this directory */
	public final path:FilePath;

	/**
		How many entries are buffered internally when reading from the directory.
		Higher numbers may improve performance, but increase memory usage.
	**/
	public var buffer:Int = 32;

	function new() {
		path = 'stub';
	}

	/**
		Read next directory entry.
		Passes `null` to `callback` if no more entries left to read.
		Ignores `.` and `..` entries.
	**/
	public function next(callback:Callback<Null<FilePath>>):Void {
		throw new NotImplementedException();
	}

	/**
		Close the directory.
	**/
	public function close(callback:Callback<NoData>):Void {
		throw new NotImplementedException();
	}
}
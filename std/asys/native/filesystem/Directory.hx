package asys.native.filesystem;

import haxe.NoData;
import haxe.exceptions.NotImplementedException;

/**
	Represents a directory.
**/
class Directory {
	/**
		How many entries are buffered internally when reading from the directory.
		Higher numbers improve performance, but increase memory usage.
	**/
	public var buffer:Int = 32;

	/**
		Read next directory entry.
	**/
	public function next(callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
	}

	/**
		Close the directory.
	**/
	public function close(callback:Callback<NoData>) {
		throw new NotImplementedException();
	}
}
package asys.io;

import haxe.io.Bytes;
import haxe.io.Encoding;
import asys.*;

/**
	Class representing an open file. Some methods in this class are instance
	variants of the same methods in `asys.FileSystem`.
**/
extern class File {
	private function get_async():AsyncFile;

	var async(get, never):AsyncFile;

	/**
		See `asys.FileSystem.chmod`.
	**/
	function chmod(mode:FilePermissions):Void;

	/**
		See `asys.FileSystem.chown`.
	**/
	function chown(uid:Int, gid:Int):Void;

	/**
		Closes the file. Any operation after this method is called is invalid.
	**/
	function close():Void;

	/**
		Same as `sync`, but metadata is not flushed unless needed for subsequent
		data reads to be correct. E.g. changes to the modification times are not
		flushed, but changes to the filesize do.
	**/
	function datasync():Void;

	/**
		Reads a part of `this` file into the given `buffer`.

		@param buffer Buffer to which data will be written.
		@param offset Position in `buffer` at which to start writing.
		@param length Number of bytes to read from `this` file.
		@param position Position in `this` file at which to start reading.
	**/
	function readBuffer(buffer:Bytes, offset:Int, length:Int, position:Int):{bytesRead:Int, buffer:Bytes};

	/**
		Reads the entire contents of `this` file.
	**/
	function readFile():Bytes;

	/**
		See `asys.FileSystem.stat`.
	**/
	function stat():FileStat;

	/**
		Flushes all modified data and metadata of `this` file to the disk.
	**/
	function sync():Void;

	/**
		See `asys.FileSystem.truncate`.
	**/
	function truncate(?len:Int = 0):Void;

	/**
		See `asys.FileSystem.utimes`.
	**/
	function utimes(atime:Date, mtime:Date):Void;

	/**
		Writes a part of the given `buffer` into `this` file.

		@param buffer Buffer from which data will be read.
		@param offset Position in `buffer` at which to start reading.
		@param length Number of bytes to write to `this` file.
		@param position Position in `this` file at which to start writing.
	**/
	function writeBuffer(buffer:Bytes, offset:Int, length:Int, position:Int):{bytesWritten:Int, buffer:Bytes};

	/**
		Writes a string to `this` file at `position`.
	**/
	function writeString(str:String, ?position:Int, ?encoding:Encoding):{bytesWritten:Int, buffer:Bytes};
}

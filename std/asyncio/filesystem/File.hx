package asyncio.filesystem;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;
import asyncio.IWritable;
import asyncio.IReadable;

class File implements IDuplex {
	/**
		Path to this file.
	**/
	public final path:FilePath;

	//TODO: this is a dummy constructor to make the compiler shut up about uninitialized finals.
	function new() {
		path = cast null;
	}

	/**
		Change file position pointer.
		The pointer position is used in read and write operations as the starting byte
		of reading or writing respectively.

		If `whence` is `SeekSet(offset)` set the pointer to the exact position
		specified by `offset`.
	 	If `whence` is `SeekEnd` move the pointer to the end-of-file.
		If `whence` is `SeekMove(offset)` move the pointer by `offset` bytes
		relative to the current position.
	**/
	public function seek(whence:FileSeek) {
		throw new NotImplemented();
	}

	/**
		Get current position pointer offset.
	**/
	public function getOffset():Int64 {
		throw new NotImplemented();
	}

	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Read up to `length` bytes and write them into `buffer` starting from `offset`
		position in `buffer`, then invoke `callback` with the amount of bytes read.
	**/
	public function read(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Force all buffered data to be written to disk.
	**/
	public function flush(callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Close the file.
	**/
	public function close(callback:Callback<NoData>):Void {
		callback.fail(new NotImplemented());
	}
}

/**
	Limits file operations to reading.
	@see asyncio.filesystem.File
**/
@:forward(path,seek,read,close)
abstract FileRead(File) from File to IReadable {}

/**
	Limits file operations to writing.
	@see asyncio.filesystem.File
**/
@:forward(path,seek,write,flush,close)
abstract FileWrite(File) from File to IWritable {}

/**
	Limits file operations to writing at the end of file.
	@see asyncio.filesystem.File
**/
@:forward(path,write,flush,close)
abstract FileAppend(File) from File to IWritable {}

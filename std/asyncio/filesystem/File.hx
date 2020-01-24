package asyncio.filesystem;

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
		Change file position indicator.
		The indicator position is used in read and write operations as the starting byte
		of reading or writing respectively.

		If `whence` is `SeekSet` set the indicator to the exact position specified by `offset`.
	 	If `whence` is `SeekEnd` move the indicator to the end-of-file.
		If `whence` is `SeekCurrent` move the indicator by `offset` bytes relative to the
		current position.
	**/
	public function seek(offset:Int, whence:FileSeek, callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}

	/**
		Write up to `length - offset` bytes from `buffer` starting from `offset`,
		then invoke `callback` with the amount of bytes written.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		callback.fail(new NotImplemented());
	}

	/**
		Read as many bytes as possible (but never more than `buffer.length - offset`)
		and write them into `buffer` starting from `offset` position in `buffer`,
		then invoke `callback` with the amount of bytes read.
	**/
	public function read(buffer:Bytes, offset:Int, callback:Callback<Int>):Void {
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
	@see aio.filesystem.File
**/
@:forward(path,seek,read,close)
abstract FileRead(File) from File to IReadable {}

/**
	Limits file operations to writing.
	@see aio.filesystem.File
**/
@:forward(path,seek,write,close)
abstract FileWrite(File) from File to IWritable {}

/**
	Limits file operations to writing at the end of file.
	@see aio.filesystem.File
**/
@:forward(path,write,close)
abstract FileAppend(File) from File to IWritable {}

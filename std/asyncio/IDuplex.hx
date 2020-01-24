package asyncio;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;

/**
	An interface to read and write bytes.
**/
interface IDuplex extends IReadable extends IWritable {
	/**
		Read as many bytes as possible (but never more than `buffer.length - offset`)
		and write them into `buffer` starting from `offset` position in `buffer`,
		then invoke `callback` with the amount of bytes read.
	**/
	function read(buffer:Bytes, offset:Int, callback:Callback<Int>):Void;

	/**
		Write up to `length - offset` bytes from `buffer` starting from `offset`,
		then invoke `callback` with the amount of bytes written.
	**/
	function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void;

	/**
		Close this duplex.
	**/
	function close(callback:Callback<NoData>):Void;
}
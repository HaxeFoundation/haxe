package asyncio;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;

/**
	An interface to write bytes into a container of bytes.
**/
interface IWritable {
	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void;

	/**
		Close this stream.
	**/
	function close(callback:Callback<NoData>):Void;
}
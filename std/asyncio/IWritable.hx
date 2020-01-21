package asyncio;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;

interface IWritable {
	/**
		Write up to `length - offset` bytes from `buffer` starting from `offset`,
		then invoke `callback` with the amount of bytes written.
	**/
	function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void;

	/**
		Close this writable.
	**/
	function close(callback:Callback<NoData>):Void;
}
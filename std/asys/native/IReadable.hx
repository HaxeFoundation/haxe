package asys.native;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;

/**
	An interface to read bytes from a source of bytes.
**/
interface IReadable {
	/**
		Read up to `length` bytes and write them into `buffer` starting from `offset`
		position in `buffer`, then invoke `callback` with the amount of bytes read.
	**/
	function read(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void;

	/**
		Close this stream.
	**/
	function close(callback:Callback<NoData>):Void;
}
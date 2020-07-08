package asys.native;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Exception;
import haxe.Callback;

/**
	An interface to write bytes into an out-going stream of bytes.
**/
interface IWritable {
	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Exception,Int>):Void;

	/**
		Force all buffered data to be committed.
	**/
	function flush(callback:Callback<Exception,NoData>):Void;

	/**
		Close this stream.
	**/
	function close(callback:Callback<Exception,NoData>):Void;
}
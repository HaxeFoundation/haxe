package aio;

import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;

interface IReadable {
	/**
		Read as many bytes as possible (but never more than `buffer.length - offset`)
		and write them into `buffer` starting from `offset` position in `buffer`,
		then invoke `callback` with the amount of bytes read.
	**/
	function read(buffer:Bytes, offset:Int, callback:Callback<Int>):Void;

	/**
		Close this readable.
	**/
	function close(callback:Callback<NoData>):Void;
}
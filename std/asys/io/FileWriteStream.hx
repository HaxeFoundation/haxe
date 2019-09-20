package asys.io;

import haxe.NoData;
import haxe.async.Signal;

extern class FileWriteStream extends haxe.io.Writable {
	final openSignal:Signal<File>;
	final readySignal:Signal<NoData>;

	var bytesWritten:Int;
	var path:String;
	var pending:Bool;
}

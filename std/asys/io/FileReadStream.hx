package asys.io;

import haxe.NoData;
import haxe.async.Signal;

typedef FileReadStreamOptions = {
	?autoClose:Bool,
	?start:Int,
	?end:Int,
	?highWaterMark:Int
};

extern class FileReadStream extends haxe.io.Readable {
	final openSignal:Signal<File>;
	final readySignal:Signal<NoData>;

	var bytesRead:Int;
	var path:String;
	var pending:Bool;
}

package asys.io;

import haxe.NoData;
import haxe.signals.Signal;

typedef FileReadStreamOptions = {
	?autoClose:Bool,
	?start:Int,
	?end:Int,
	?highWaterMark:Int
};

//TODO: why does extern class extends non-extern one?
extern class FileReadStream extends haxe.io.Readable {
	var openSignal(get,never):Signal<File>;
	var readySignal(get,never):Signal<NoData>;

	var bytesRead:Int;
	var path:String;
	var pending:Bool;
}

package asys.io;

import haxe.NoData;
import haxe.signals.Signal;

//TODO: why does extern class extends non-extern one?
extern class FileWriteStream extends haxe.io.Writable {
	var openSignal(get,never):Signal<File>;
	var readySignal(get,never):Signal<NoData>;

	var bytesWritten:Int;
	var path:String;
	var pending:Bool;
}

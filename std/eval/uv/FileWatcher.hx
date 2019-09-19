package eval.uv;

import haxe.async.*;
import haxe.io.FilePath;

extern class FileWatcher {
	function new(filename:FilePath, recursive:Bool, cb:Callback<asys.FileWatcherEvent>);
	function close(cb:Callback<haxe.NoData>):Void;
	function ref():Void;
	function unref():Void;
}

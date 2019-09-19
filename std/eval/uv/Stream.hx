package eval.uv;

import haxe.NoData;
import haxe.async.Callback;
import haxe.io.Bytes;
import asys.net.*;

extern class Stream {
	function write(data:Bytes, cb:Callback<NoData>):Void;
	function end(cb:Callback<NoData>):Void;
	function startRead(cb:Callback<Bytes>):Void;
	function stopRead():Void;
	function listen(backlog:Int, cb:Callback<NoData>):Void;
	function close(cb:Callback<NoData>):Void;
	function ref():Void;
	function unref():Void;
}

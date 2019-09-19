package eval.uv;

import haxe.NoData;
import haxe.async.Callback;
import haxe.io.Bytes;
import asys.net.*;

extern class Pipe {
	function new(ipc:Bool);
	function open(fd:Int):Void;
	function connectIpc(path:String, cb:Callback<NoData>):Void;
	function bindIpc(path:String):Void;
	function accept():Pipe;
	function writeHandle(data:Bytes, handle:eval.uv.Stream, cb:Callback<NoData>):Void;
	function pendingCount():Int;
	function acceptPending():PipeAccept;
	function getSockName():SocketAddress;
	function getPeerName():SocketAddress;
	function asStream():Stream;
}

enum PipeAccept {
	Socket(_:eval.uv.Socket);
	Pipe(_:eval.uv.Pipe);
}

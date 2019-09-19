package eval.uv;

import haxe.NoData;
import haxe.async.*;

extern class Process {
	function new(
		exitCb:Callback<{code:Int, signal:Int}>,
		file:String,
		args:Array<String>,
		env:Array<String>,
		cwd:String,
		flags:asys.uv.UVProcessSpawnFlags,
		stdio:Array<ProcessIO>,
		uid:Int,
		gid:Int
	);
	function kill(signal:Int):Void;
	function getPid():Int;
	function close(cb:Callback<NoData>):Void;
	function ref():Void;
	function unref():Void;
}

enum ProcessIO {
	Ignore;
	Inherit;
	Pipe(readable:Bool, writable:Bool, pipe:eval.uv.Stream);
	Ipc(pipe:eval.uv.Stream);
	// Stream(_);
	// Fd(_);
}

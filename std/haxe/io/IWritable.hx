package haxe.io;

import haxe.NoData;
import haxe.async.Signal;

interface IWritable {
	final drainSignal:Signal<NoData>;
	final finishSignal:Signal<NoData>;
	final pipeSignal:Signal<IReadable>;
	final unpipeSignal:Signal<IReadable>;
	function write(chunk:Bytes):Bool;
	function end():Void;
	function cork():Void;
	function uncork():Void;
}

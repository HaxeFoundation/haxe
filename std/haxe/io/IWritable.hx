package haxe.io;

import haxe.NoData;
import haxe.signals.Signal;

interface IWritable {
	var drainSignal(get,never):Signal<NoData>;
	var finishSignal(get,never):Signal<NoData>;
	var pipeSignal(get,never):Signal<IReadable>;
	var unpipeSignal(get,never):Signal<IReadable>;
	function write(chunk:Bytes):Bool;
	function end():Void;
	function cork():Void;
	function uncork():Void;
}

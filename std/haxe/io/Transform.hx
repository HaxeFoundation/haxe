package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.signals.*;

@:access(haxe.io.Readable)
@:access(haxe.io.Writable)
class Transform implements IReadable implements IWritable {
	public var dataSignal(get,never):Signal<Bytes>;
	function get_dataSignal() return output.dataSignal;

	public var endSignal(get,never):Signal<NoData>;
	function get_endSignal() return output.endSignal;

	public var errorSignal(get,never):Signal<Error>;
	function get_errorSignal() return output.errorSignal;

	public var pauseSignal(get,never):Signal<NoData>;
	function get_pauseSignal() return output.pauseSignal;

	public var resumeSignal(get,never):Signal<NoData>;
	function get_resumeSignal() return output.resumeSignal;

	public var drainSignal(get,never):Signal<NoData>;
	function get_drainSignal() return input.drainSignal;

	public var finishSignal(get,never):Signal<NoData>;
	function get_finishSignal() return input.finishSignal;

	public var pipeSignal(get,never):Signal<IReadable>;
	function get_pipeSignal() return input.pipeSignal;

	public var unpipeSignal(get,never):Signal<IReadable>;
	function get_unpipeSignal() return input.unpipeSignal;

	final input:Writable;
	final output:Readable;

	var transforming:Bool = false;

	function new() {
		input = new TransformWritable(this);
		output = @:privateAccess new Readable(0);
	}

	function internalTransform(chunk:Bytes):Void {
		throw "not implemented";
	}

	function push(chunk:Bytes):Void {
		transforming = false;
		output.asyncRead([chunk], false);
		input.internalWrite();
	}

	public inline function write(chunk:Bytes):Bool {
		return input.write(chunk);
	}

	public function end():Void {
		input.end();
		output.asyncRead(null, true);
	}

	public inline function pause():Void {
		output.pause();
	}

	public inline function resume():Void {
		output.resume();
	}

	public inline function pipe(to:IWritable):Void {
		output.pipe(to);
	}

	public inline function cork():Void {
		input.cork();
	}

	public inline function uncork():Void {
		input.uncork();
	}
}

@:access(haxe.io.Transform)
private class TransformWritable extends Writable {
	final parent:Transform;

	public function new(parent:Transform) {
		this.parent = parent;
	}

	override function internalWrite():Void {
		if (buffer.length > 0) {
			parent.transforming = true;
			parent.internalTransform(pop());
		}
	}
}

package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.async.Signal;

@:access(haxe.io.Readable)
@:access(haxe.io.Writable)
class Transform implements IReadable implements IWritable {
	public final dataSignal:Signal<Bytes>;
	public final endSignal:Signal<NoData>;
	public final errorSignal:Signal<Error>;
	public final pauseSignal:Signal<NoData>;
	public final resumeSignal:Signal<NoData>;

	public final drainSignal:Signal<NoData>;
	public final finishSignal:Signal<NoData>;
	public final pipeSignal:Signal<IReadable>;
	public final unpipeSignal:Signal<IReadable>;

	final input:Writable;
	final output:Readable;

	var transforming:Bool = false;

	function new() {
		input = new TransformWritable(this);
		output = @:privateAccess new Readable(0);
		dataSignal = output.dataSignal;
		endSignal = output.endSignal;
		errorSignal = output.errorSignal;
		pauseSignal = output.pauseSignal;
		resumeSignal = output.resumeSignal;
		drainSignal = input.drainSignal;
		finishSignal = input.finishSignal;
		pipeSignal = input.pipeSignal;
		unpipeSignal = input.unpipeSignal;
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

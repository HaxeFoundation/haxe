package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.ds.List;
import haxe.io.Readable.ReadResult;

/**
	A stream which is both readable and writable.

	This is an abstract base class that should never be used directly. Instead,
	child classes should override the `internalRead` and `internalWrite` methods.
	See `haxe.io.Readable` and `haxe.io.Writable`.
**/
@:access(haxe.io.Readable)
@:access(haxe.io.Writable)
class Duplex implements IReadable implements IWritable {
	public final dataSignal:Signal<Bytes>;
	public final endSignal:Signal<NoData>;
	public final errorSignal:Signal<Error<Any>>;
	public final pauseSignal:Signal<NoData>;
	public final resumeSignal:Signal<NoData>;

	public final drainSignal:Signal<NoData>;
	public final finishSignal:Signal<NoData>;
	public final pipeSignal:Signal<IReadable>;
	public final unpipeSignal:Signal<IReadable>;

	final input:Writable;
	final output:Readable;
	final inputBuffer:List<Bytes>;
	final outputBuffer:List<Bytes>;

	function get_inputBufferLength() {
		return input.bufferLength;
	}
	var inputBufferLength(get, never):Int;

	function get_outputBufferLength() {
		return output.bufferLength;
	}
	var outputBufferLength(get, never):Int;

	function new() {
		input = new DuplexWritable(this);
		output = new DuplexReadable(this);
		dataSignal = output.dataSignal;
		endSignal = output.endSignal;
		errorSignal = output.errorSignal;
		pauseSignal = output.pauseSignal;
		resumeSignal = output.resumeSignal;
		drainSignal = input.drainSignal;
		finishSignal = input.finishSignal;
		pipeSignal = input.pipeSignal;
		unpipeSignal = input.unpipeSignal;
		inputBuffer = input.buffer;
		outputBuffer = output.buffer;
	}

	// override by implementing classes
	function internalRead(remaining:Int):ReadResult {
		throw "not implemented";
	}

	function internalWrite():Void {
		throw "not implemented";
	}

	inline function pop():Bytes {
		return input.pop();
	}

	inline function push(chunk:Bytes):Void {
		output.push(chunk);
	}

	inline function asyncRead(chunks:Array<Bytes>, eof:Bool):Void {
		output.asyncRead(chunks, eof);
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

@:access(haxe.io.Duplex)
private class DuplexWritable extends Writable {
	final parent:Duplex;

	public function new(parent:Duplex) {
		this.parent = parent;
	}

	override function internalWrite():Void {
		parent.internalWrite();
	}
}

@:access(haxe.io.Duplex)
private class DuplexReadable extends Readable {
	final parent:Duplex;

	public function new(parent:Duplex) {
		super();
		this.parent = parent;
	}

	override function internalRead(remaining):ReadResult {
		return parent.internalRead(remaining);
	}
}

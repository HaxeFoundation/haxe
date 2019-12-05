package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.signals.Signal;
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
	public var dataSignal(get,never):Signal<Bytes>;
	function get_dataSignal():Signal<Bytes>
		return output.dataSignal;

	public var endSignal(get,never):Signal<NoData>;
	function get_endSignal():Signal<NoData>
		return output.endSignal;

	public var errorSignal(get,never):Signal<Error>;
	function get_errorSignal():Signal<Error>
		return output.errorSignal;

	public var pauseSignal(get,never):Signal<NoData>;
	function get_pauseSignal():Signal<NoData>
		return output.pauseSignal;

	public var resumeSignal(get,never):Signal<NoData>;
	function get_resumeSignal():Signal<NoData>
		return output.resumeSignal;

	public var drainSignal(get,never):Signal<NoData>;
	function get_drainSignal():Signal<NoData>
		return input.drainSignal;

	public var finishSignal(get,never):Signal<NoData>;
	function get_finishSignal():Signal<NoData>
		return input.finishSignal;

	public var pipeSignal(get,never):Signal<IReadable>;
	function get_pipeSignal():Signal<IReadable>
		return input.pipeSignal;

	public var unpipeSignal(get,never):Signal<IReadable>;
	function get_unpipeSignal():Signal<IReadable>
		return input.unpipeSignal;


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

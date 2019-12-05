package haxe.io;

import haxe.NoData;
import haxe.async.*;
import haxe.signals.*;
import haxe.ds.List;

/**
	A writable stream.

	This is an abstract base class that should never be used directly. Instead,
	subclasses should override the `internalWrite` method.
**/
class Writable implements IWritable {
	public var drainSignal(get,never):Signal<NoData>;
	final _drainSignal = new ArraySignal<NoData>();
	function get_drainSignal():Signal<NoData>
		return _drainSignal;

	public var finishSignal(get,never):Signal<NoData>;
	final _finishSignal = new ArraySignal<NoData>();
	function get_finishSignal():Signal<NoData>
		return _finishSignal;

	public var pipeSignal(get,never):Signal<IReadable>;
	final _pipeSignal = new ArraySignal<IReadable>();
	function get_pipeSignal():Signal<IReadable>
		return _pipeSignal;

	public var unpipeSignal(get,never):Signal<IReadable>;
	final _unpipeSignal = new ArraySignal<IReadable>();
	function get_unpipeSignal():Signal<IReadable>
		return _unpipeSignal;


	public var highWaterMark = 8192;
	public var bufferLength(default, null) = 0;
	public var corkCount(default, null) = 0;
	public var done(default, null) = false;

	var willDrain = false;
	var willFinish = false;
	var deferred:asys.Timer;
	var buffer = new List<Bytes>();

	// for use by implementing classes
	function pop():Bytes {
		var chunk = buffer.pop();
		bufferLength -= chunk.length;
		if (willDrain && buffer.length == 0) {
			willDrain = false;
			if (deferred == null)
				deferred = Defer.nextTick(() -> {
					deferred = null;
					_drainSignal.emit(new NoData());
				});
		}
		if (willFinish && buffer.length == 0) {
			willFinish = false;
			Defer.nextTick(() -> _finishSignal.emit(new NoData()));
		}
		return chunk;
	}

	// override by implementing classes
	function internalWrite():Void {
		throw "not implemented";
	}

	// for producers
	public function write(chunk:Bytes):Bool {
		if (done)
			throw "stream already done";
		buffer.add(chunk);
		bufferLength += chunk.length;
		if (corkCount <= 0)
			internalWrite();
		if (bufferLength >= highWaterMark) {
			willDrain = true;
			return false;
		}
		return true;
	}

	public function end():Void {
		corkCount = 0;
		if (buffer.length > 0)
			internalWrite();
		if (buffer.length > 0)
			willFinish = true;
		else
			_finishSignal.emit(new NoData());
		done = true;
	}

	public function cork():Void {
		if (done)
			return;
		corkCount++;
	}

	public function uncork():Void {
		if (done || corkCount <= 0)
			return;
		if (--corkCount == 0 && buffer.length > 0)
			internalWrite();
	}
}

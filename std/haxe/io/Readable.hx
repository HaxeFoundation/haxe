package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.signals.*;
import haxe.async.*;
import haxe.ds.List;

/**
	A readable stream.

	This is an abstract base class that should never be used directly. Instead,
	subclasses should override the `internalRead` method.
**/
class Readable implements IReadable {
	/**
		See `IReadable.dataSignal`.
	**/
	public var dataSignal(get,never):Signal<Bytes>;
	final _dataSignal = new WrappedSignal<Bytes>();
	function get_dataSignal():Signal<Bytes>
		return _dataSignal;

	/**
		See `IReadable.endSignal`.
	**/
	public var endSignal(get,never):Signal<NoData>;
	final _endSignal = new WrappedSignal<NoData>();
	function get_endSignal():Signal<NoData>
		return _endSignal;

	/**
		See `IReadable.errorSignal`.
	**/
	public var errorSignal(get,never):Signal<Error>;
	final _errorSignal = new ArraySignal();
	function get_errorSignal():Signal<Error>
		return _errorSignal;

	/**
		See `IReadable.pauseSignal`.
	**/
	public var pauseSignal(get,never):Signal<NoData>;
	final _pauseSignal = new ArraySignal();
	function get_pauseSignal():Signal<NoData>
		return _pauseSignal;

	/**
		See `IReadable.resumeSignal`.
	**/
	public var resumeSignal(get,never):Signal<NoData>;
	final _resumeSignal = new ArraySignal();
	function get_resumeSignal():Signal<NoData>
		return _resumeSignal;

	/**
		High water mark. `Readable` will call `internalRead` pre-emptively to fill
		up the internal buffer up to this value when possible. Set to `0` to
		disable pre-emptive reading.
	**/
	public var highWaterMark = 8192;

	/**
		Total amount of data currently in the internal buffer, in bytes.
	**/
	public var bufferLength(default, null) = 0;

	/**
		Whether data is flowing at the moment. When flowing, data signals will be
		emitted and the internal buffer will be empty.
	**/
	public var flowing(default, null) = false;

	/**
		Whether this stream is finished. When `true`, no further signals will be
		emmited by `this` instance.
	**/
	public var done(default, null) = false;

	var buffer = new List<Bytes>();
	var deferred:asys.Timer;
	var willEof = false;

	@:dox(show)
	function new(?highWaterMark:Int = 8192) {
		this.highWaterMark = highWaterMark;
		_dataSignal.changeSignal.on(() -> {
			if (_dataSignal.listenerCount > 0)
				resume();
		});
		_endSignal.changeSignal.on(() -> {
			if (_endSignal.listenerCount > 0)
				resume();
		});
	}

	inline function shouldFlow():Bool {
		return !done && (dataSignal.listenerCount > 0 || endSignal.listenerCount > 0);
	}

	function process():Void {
		deferred = null;
		if (!shouldFlow())
			flowing = false;
		if (!flowing)
			return;

		var reschedule = false;

		// pre-emptive read until HWM
		if (!willEof && !done)
			while (bufferLength < highWaterMark) {
				switch (internalRead(highWaterMark - bufferLength)) {
					case None:
						break;
					case Data(chunks, eof):
						reschedule = true;
						for (chunk in chunks)
							push(chunk);
						if (eof) {
							willEof = true;
							break;
						}
				}
			}

		// emit data
		while (buffer.length > 0 && flowing && shouldFlow()) {
			reschedule = true;
			_dataSignal.emit(pop());
		}

		if (willEof) {
			_endSignal.emit(new NoData());
			flowing = false;
			done = true;
			return;
		}

		if (!shouldFlow())
			flowing = false;
		else if (reschedule)
			scheduleProcess();
	}

	inline function scheduleProcess():Void {
		if (deferred == null)
			deferred = Defer.nextTick(process);
	}

	function push(chunk:Bytes):Bool {
		if (done)
			throw "stream already done";
		buffer.add(chunk);
		bufferLength += chunk.length;
		return bufferLength < highWaterMark;
	}

	/**
		This method should be used internally from `internalRead` to provide data
		resulting from asynchronous operations. The arguments to this method are
		the same as `ReadableResult.Data`. See `internalRead` for more details.
	**/
	@:dox(show)
	function asyncRead(chunks:Array<Bytes>, eof:Bool):Void {
		if (done || willEof)
			throw "stream already done";
		if (chunks != null)
			for (chunk in chunks)
				push(chunk);
		if (eof)
			willEof = true;
		if (chunks != null || eof)
			scheduleProcess();
	}

	function pop():Bytes {
		if (done)
			throw "stream already done";
		var chunk = buffer.pop();
		bufferLength -= chunk.length;
		return chunk;
	}

	/**
		This method should be overridden by a subclass.

		This method will be called as needed by `Readable`. The `remaining`
		argument is an indication of how much data is needed to fill the internal
		buffer up to the high water mark, or the current requested amount of data.
		This method is called in a cycle until the read cycle is stopped with a
		`None` return or an EOF is indicated, as described below.

		If a call to this method returns `None`, the current read cycle is
		ended. This value should be returned when there is no data available at the
		moment, but a read request was scheduled and will later be fulfilled by a
		call to `asyncRead`.

		If a call to this method returns `Data(chunks, eof)`, `chunks` will be
		added to the internal buffer. If `eof` is `true`, the read cycle is ended
		and the readable stream signals an EOF (end-of-file). After an EOF, no
		further calls will be made. `chunks` should not be an empty array if `eof`
		is `false`.

		Code inside this method should only call `asyncRead` (asynchronously from
		a callback) or provide data using the return value.
	**/
	@:dox(show)
	function internalRead(remaining:Int):ReadResult {
		throw "not implemented";
	}

	/**
		See `IReadable.resume`.
	**/
	public function resume():Void {
		if (done)
			return;
		if (!flowing) {
			_resumeSignal.emit(new NoData());
			flowing = true;
			scheduleProcess();
		}
	}

	/**
		See `IReadable.pause`.
	**/
	public function pause():Void {
		if (done)
			return;
		if (flowing) {
			_pauseSignal.emit(new NoData());
			flowing = false;
		}
	}

	/**
		See `IReadable.pipe`.
	**/
	public function pipe(to:IWritable):Void {
		throw "!";
	}
}

/**
	See `Readable.internalRead`.
**/
enum ReadResult {
	None;
	Data(chunks:Array<Bytes>, eof:Bool);
}

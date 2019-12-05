package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.signals.Signal;

/**
	A readable stream.

	This interface should be used wherever an object that is readable is
	expected, regardless of a specific implementation. See `Readable` for an
	abstract base class that can be used to implement an `IReadable`.
**/
interface IReadable {
	/**
		Emitted whenever a chunk of data is available.
	**/
	var dataSignal(get,never):Signal<Bytes>;

	/**
		Emitted when the stream is finished. No further signals will be emitted by
		`this` instance after `endSignal` is emitted.
	**/
	var endSignal(get,never):Signal<NoData>;

	/**
		Emitted for any error that occurs during reading.
	**/
	var errorSignal(get,never):Signal<Error>;

	/**
		Emitted when `this` stream is paused.
	**/
	var pauseSignal(get,never):Signal<NoData>;

	/**
		Emitted when `this` stream is resumed.
	**/
	var resumeSignal(get,never):Signal<NoData>;

	/**
		Resumes flow of data. Note that this method is called automatically
		whenever listeners to either `dataSignal` or `endSignal` are added.
	**/
	function resume():Void;

	/**
		Pauses flow of data.
	**/
	function pause():Void;

	/**
		Pipes the data from `this` stream to `target`.
	**/
	function pipe(target:IWritable):Void;

	/**
		Indicates to `this` stream that an additional `amount` bytes should be read
		from the underlying data source. Note that the actual data will arrive via
		`dataSignal`.
	**/
	// function read(amount:Int):Void;
}

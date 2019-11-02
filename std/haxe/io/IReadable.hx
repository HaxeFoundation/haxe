package haxe.io;

import haxe.Error;
import haxe.NoData;
import haxe.async.Signal;

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
	final dataSignal:Signal<Bytes>;

	/**
		Emitted when the stream is finished. No further signals will be emitted by
		`this` instance after `endSignal` is emitted.
	**/
	final endSignal:Signal<NoData>;

	/**
		Emitted for any error that occurs during reading.
	**/
	final errorSignal:Signal<Error>;

	/**
		Emitted when `this` stream is paused.
	**/
	final pauseSignal:Signal<NoData>;

	/**
		Emitted when `this` stream is resumed.
	**/
	final resumeSignal:Signal<NoData>;

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

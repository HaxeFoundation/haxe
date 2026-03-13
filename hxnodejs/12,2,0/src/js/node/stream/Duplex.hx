/*
 * Copyright (C)2014-2020 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package js.node.stream;

import haxe.extern.EitherType;
import js.node.events.EventEmitter.Event;
import js.node.stream.Readable.IReadable;
import js.node.stream.Writable.IWritable;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Writable streams are an abstraction for a destination to which data is written.

	@see https://nodejs.org/api/stream.html#stream_writable_streams
**/
enum abstract DuplexEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	// Writable stream events -------------------------------------------------
	// var Close:DuplexEvent<Void->Void> = "close";

	/**
		If a call to stream.write(chunk) returns `false`, the `'drain'` event will be emitted
		when it is appropriate to resume writing data to the stream.

		@see https://nodejs.org/api/stream.html#stream_event_drain
	**/
	var Drain:DuplexEvent<Void->Void> = "drain";

	// var Error:DuplexEvent<Error->Void> = "error";

	/**
		The `'finish'` event is emitted after the stream.end() method has been called,
		and all data has been flushed to the underlying system.

		@see https://nodejs.org/api/stream.html#stream_event_finish
	**/
	var Finish:DuplexEvent<Void->Void> = "finish";

	/**
		The `'pipe'` event is emitted when the stream.pipe() method is called on a readable stream,
		adding this writable to its set of destinations.

		@see https://nodejs.org/api/stream.html#stream_event_pipe
	**/
	var Pipe:DuplexEvent<IReadable->Void> = "pipe";

	/**
		The `'unpipe'` event is emitted when the stream.unpipe() method is called on a Readable stream,
		removing this Writable from its set of destinations.

		@see https://nodejs.org/api/stream.html#stream_event_unpipe
	**/
	var Unpipe:DuplexEvent<IReadable->Void> = "unpipe";

	// Readable stream events -------------------------------------------------
	// var Close:DuplexEvent<Void->Void> = "close";

	/**
		The `'data'` event is emitted whenever the stream is relinquishing ownership of
		a chunk of data to a consumer. This may occur whenever the stream is switched
		in flowing mode by calling `readable.pipe()`, `readable.resume()`, or by
		attaching a listener callback to the `'data'` event. The `'data'` event will
		also be emitted whenever the `readable.read()` method is called and a chunk of
		data is available to be returned.

		@see https://nodejs.org/api/stream.html#stream_event_data
	**/
	var Data:DuplexEvent<Dynamic->Void> = "data";

	/**
		The `'end'` event is emitted when there is no more data to be consumed from
		the stream.

		@see https://nodejs.org/api/stream.html#stream_event_end
	**/
	var End:DuplexEvent<Void->Void> = "end";

	// var Error:DuplexEvent<Error->Void> = "error";

	/**
		The `'pause'` event is emitted when `stream.pause()` is called
		and `readableFlowing` is not `false`.

		@see https://nodejs.org/api/stream.html#stream_event_pause
	**/
	var Pause:DuplexEvent<Void->Void> = "pause";

	/**
		The `'readable'` event is emitted when there is data available to be read from
		the stream. In some cases, attaching a listener for the `'readable'` event will
		cause some amount of data to be read into an internal buffer.

		@see https://nodejs.org/api/stream.html#stream_event_readable
	**/
	var Readable:DuplexEvent<Void->Void> = "readable";

	/**
		The `'resume'` event is emitted when `stream.resume()` is
		called and `readableFlowing` is not `true`.

		@see https://nodejs.org/api/stream.html#stream_event_resume
	**/
	var Resume:DuplexEvent<Void->Void> = "resume";

	// Overlapped events ------------------------------------------------------

	/**
		The `'close'` event is emitted when the stream and any of its underlying
		resources (a file descriptor, for example) have been closed.
		The event indicates that no more events will be emitted, and no further computation will occur.

		@see https://nodejs.org/api/stream.html#stream_event_close
		@see https://nodejs.org/api/stream.html#stream_event_close_1
	**/
	var Close:DuplexEvent<Void->Void> = "close";

	/**
		@see https://nodejs.org/api/stream.html#stream_event_error
		@see https://nodejs.org/api/stream.html#stream_event_error_1
	**/
	var Error:DuplexEvent<Error->Void> = "error";
}

/**
	Duplex streams are streams that implement both the `Readable` and `Writable` interfaces.

	@see https://nodejs.org/api/stream.html#stream_class_stream_duplex
**/
@:jsRequire("stream", "Duplex")
extern class Duplex<TSelf:Duplex<TSelf>> extends Readable<TSelf> implements IDuplex {
	// --------- Writable interface implementation ----------------------------

	/**
		The `writable.cork()` method forces all written data to be buffered in memory.
		The buffered data will be flushed when either the `stream.uncork()` or `stream.end()` methods are called.

		@see https://nodejs.org/api/stream.html#stream_writable_cork
	**/
	function cork():Void;

	// This field is defined in super class.
	// function destroy(?error:Error):TSelf;
	// var destroyed(default, null):Bool;

	/**
		Calling the `writable.end()` method signals that no more data will be written to the Writable.
		The optional `chunk` and `encoding` arguments allow one final additional chunk of data to be written immediately before closing the stream.
		If provided, the optional `callback` function is attached as a listener for the 'finish' event.

		@see https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
	**/
	@:overload(function(?callback:EitherType<Void->Void, Null<Error>->Void>):Void {})
	function end(chunk:Dynamic, ?encoding:String, ?callback:EitherType<Void->Void, Null<Error>->Void>):Void;

	/**
		The `writable.setDefaultEncoding()` method sets the default `encoding` for a Writable stream.

		@see https://nodejs.org/api/stream.html#stream_writable_setdefaultencoding_encoding
	**/
	function setDefaultEncoding(encoding:String):TSelf;

	/**
		The `writable.uncork()` method flushes all data buffered since `stream.cork()` was called.

		@see https://nodejs.org/api/stream.html#stream_writable_uncork
	**/
	function uncork():Void;

	/**
		Is `true` if it is safe to call `writable.write()`.

		@see https://nodejs.org/api/stream.html#stream_writable_writable
	**/
	var writable(default, null):Bool;

	/**
		Is `true` after `writable.end()` has been called. This property
		does not indicate whether the data has been flushed, for this use
		`writable.writableFinished` instead.

		@see https://nodejs.org/api/stream.html#stream_writable_writableended
	**/
	var writableEnded(default, null):Bool;

	/**
		Is set to `true` immediately before the 'finish' event is emitted.

		@see https://nodejs.org/api/stream.html#stream_writable_writablefinished
	**/
	var writableFinished(default, null):Bool;

	/**
		Return the value of `highWaterMark` passed when constructing this `Writable`.

		@see https://nodejs.org/api/stream.html#stream_writable_writablehighwatermark
	**/
	var writablehighWaterMark(default, null):Int;

	/**
		This property contains the number of bytes (or objects) in the queue ready to be written.
		The value provides introspection data regarding the status of the `highWaterMark`.

		@see https://nodejs.org/api/stream.html#stream_writable_writablelength
	**/
	var writableLength(default, null):Int;

	/**
		Getter for the property `objectMode` of a given `Writable` stream.

		@see https://nodejs.org/api/stream.html#stream_writable_writableobjectmode
	**/
	var writableObjectMode(default, null):Bool;

	/**
		The `writable.write()` method writes some data to the stream, and calls the supplied `callback` once the data has been fully handled.
		If an error occurs, the `callback` may or may not be called with the error as its first argument.
		To reliably detect write errors, add a listener for the `'error'` event.

		@see https://nodejs.org/api/stream.html#stream_writable_write_chunk_encoding_callback
	**/
	function write(chunk:Dynamic, ?encoding:String, ?callback:EitherType<Void->Void, Null<Error>->Void>):Bool;

	// --------- API for implementing a Writable Stream -----------------------
	// function new(?options:DuplexNewOptions);

	/**
		All `Writable` stream implementations must provide a `writable._write()` method to send data to the underlying resource.

		@see https://nodejs.org/api/stream.html#stream_writable_write_chunk_encoding_callback_1
	**/
	private function _write(chunk:Dynamic, encoding:String, callback:Null<Error>->Void):Void;

	/**
		This function MUST NOT be called by application code directly.
		It should be implemented by child classes, and called by the internal `Writable` class methods only.

		@see https://nodejs.org/api/stream.html#stream_writable_writev_chunks_callback
	**/
	private function _writev(chunks:Array<Writable.Chunk>, callback:Null<Error>->Void):Void;

	// This field is defined in super class.
	// private function _destroy(err:Null<Error>, ?callback:Null<Error>->Void):Void;

	/**
		The `_final()` method must not be called directly.
		t may be implemented by child classes, and if so, will be called by the internal `Writable` class methods only.

		@see https://nodejs.org/api/stream.html#stream_writable_final_callback
	**/
	private function _final(callback:Null<Error>->Void):Void;

	// --------- Overlapped interface -----------------------------------------

	/**
		Destroy the stream.
		Optionally emit an `'error'` event, and emit a `'close'` event unless `emitClose` is set in `false`.

		@see https://nodejs.org/api/stream.html#stream_writable_destroy_error
		@see https://nodejs.org/api/stream.html#stream_readable_destroy_error
	**/
	override function destroy(?error:Error):TSelf;

	// This field is defined in super class.
	// var destroyed(default, null):Bool;

	/**
		@see https://nodejs.org/api/stream.html#stream_constructor_new_stream_writable_options
		@see https://nodejs.org/api/stream.html#stream_new_stream_readable_options
	**/
	function new(?options:DuplexNewOptions);

	/**
		The `_destroy()` method is called by `destroy()`.
		It can be overridden by child classes but it **must not** be called directly.

		@see https://nodejs.org/api/stream.html#stream_writable_destroy_err_callback
		@see https://nodejs.org/api/stream.html#stream_readable_destroy_err_callback
	**/
	private override function _destroy(err:Null<Error>, callback:Null<Error>->Void):Void;

	// This field is defined in super class.
	// var isTTY(default, null):Bool;
}

/**
	Passed to both `Writable` and `Readable` constructors. Also has the following fields:

	@see https://nodejs.org/api/stream.html#stream_new_stream_duplex_options
**/
typedef DuplexNewOptions = {
	> Readable.ReadableNewOptions,
	> Writable.WritableNewOptions,

	/**
		If set to `false`, then the stream will automatically end the writable side when the readable side ends. Default: `true`.
	**/
	@:optional var allowHalfOpen:Bool;

	/**
		Sets `objectMode` for readable side of the stream. Has no effect if `objectMode` is `true`. Default: `false`.
	**/
	@:optional var readableObjectMode:Bool;

	/**
		Sets `objectMode` for writable side of the stream. Has no effect if `objectMode` is `true`. Default: `false`.
	**/
	@:optional var writableObjectMode:Bool;

	/**
		Sets `highWaterMark` for the readable side of the stream. Has no effect if `highWaterMark` is provided.
	**/
	@:optional var readableHighWaterMark:Int;

	/**
		Sets `highWaterMark` for the writable side of the stream. Has no effect if `highWaterMark` is provided.
	**/
	@:optional var writableHighWaterMark:Int;
}

@:remove
extern interface IDuplex extends IReadable extends IWritable {}

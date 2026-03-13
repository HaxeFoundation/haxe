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

import js.node.Iterator;
import js.node.Stream;
import js.node.events.EventEmitter.Event;
import js.node.stream.Writable.IWritable;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Readable streams are an abstraction for a source from which data is consumed.

	@see https://nodejs.org/api/stream.html#stream_readable_streams
**/
enum abstract ReadableEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		The `'close'` event is emitted when the stream and any of its underlying
		resources (a file descriptor, for example) have been closed.
		The event indicates that no more events will be emitted, and no further computation will occur.

		@see https://nodejs.org/api/stream.html#stream_event_close_1
	**/
	var Close:ReadableEvent<Void->Void> = "close";

	/**
		The `'data'` event is emitted whenever the stream is relinquishing ownership of
		a chunk of data to a consumer. This may occur whenever the stream is switched
		in flowing mode by calling `readable.pipe()`, `readable.resume()`, or by
		attaching a listener callback to the `'data'` event. The `'data'` event will
		also be emitted whenever the `readable.read()` method is called and a chunk of
		data is available to be returned.

		@see https://nodejs.org/api/stream.html#stream_event_data
	**/
	var Data:ReadableEvent<Dynamic->Void> = "data";

	/**
		The `'end'` event is emitted when there is no more data to be consumed from
		the stream.

		@see https://nodejs.org/api/stream.html#stream_event_end
	**/
	var End:ReadableEvent<Void->Void> = "end";

	/**
		The `'error'` event may be emitted by a `Readable` implementation at any time.
		Typically, this may occur if the underlying stream is unable to generate data
		due to an underlying internal failure, or when a stream implementation attempts
		to push an invalid chunk of data.

		@see https://nodejs.org/api/stream.html#stream_event_error_1
	**/
	var Error:ReadableEvent<Error->Void> = "error";

	/**
		The `'pause'` event is emitted when `stream.pause()` is called
		and `readableFlowing` is not `false`.

		@see https://nodejs.org/api/stream.html#stream_event_pause
	**/
	var Pause:ReadableEvent<Void->Void> = "pause";

	/**
		The `'readable'` event is emitted when there is data available to be read from
		the stream. In some cases, attaching a listener for the `'readable'` event will
		cause some amount of data to be read into an internal buffer.

		@see https://nodejs.org/api/stream.html#stream_event_readable
	**/
	var Readable:ReadableEvent<Void->Void> = "readable";

	/**
		The `'resume'` event is emitted when `stream.resume()` is
		called and `readableFlowing` is not `true`.

		@see https://nodejs.org/api/stream.html#stream_event_resume
	**/
	var Resume:ReadableEvent<Void->Void> = "resume";
}

/**
	@see https://nodejs.org/api/stream.html#stream_class_stream_readable
**/
@:jsRequire("stream", "Readable")
extern class Readable<TSelf:Readable<TSelf>> extends Stream<TSelf> implements IReadable {
	/**
		Destroy the stream. Optionally emit an `'error'` event, and emit a `'close'` event unless `emitClose` is set in `false`.
		After this call, the readable stream will release any internal resources and subsequent calls to `push()` will be ignored.
		Implementors should not override this method, but instead implement `readable._destroy()`.

		@see https://nodejs.org/api/stream.html#stream_readable_destroy_error
	**/
	function destroy(?error:Error):TSelf;

	/**
		Is `true` after `readable.destroy()` has been called.

		@see https://nodejs.org/api/stream.html#stream_readable_destroyed
	**/
	var destroyed(default, null):Bool;

	/**
		The `readable.isPaused()` method returns the current operating state of the `Readable`.
		This is used primarily by the mechanism that underlies the `readable.pipe()` method.
		In most typical cases, there will be no reason to use this method directly.

		@see https://nodejs.org/api/stream.html#stream_readable_ispaused
	**/
	function isPaused():Bool;

	/**
		The `readable.pause()` method will cause a stream in flowing mode to stop emitting `'data'` events,
		switching out of flowing mode. Any data that becomes available will remain in the internal buffer.

		@see https://nodejs.org/api/stream.html#stream_readable_pause
	**/
	function pause():TSelf;

	/**
		The `readable.pipe()` method attaches a `Writable` stream to the `readable`,
		causing it to switch automatically into flowing mode and push all of its data to the attached `Writable`.
		The flow of data will be automatically managed so that the destination `Writable` stream
		is not overwhelmed by a faster `Readable` stream.

		@see https://nodejs.org/api/stream.html#stream_readable_pipe_destination_options
	**/
	function pipe<T:IWritable>(destination:T, ?options:{?end:Bool}):T;

	/**
		The `readable.read()` method pulls some data out of the internal buffer and returns it.
		If no data available to be read, `null` is returned. By default,
		the data will be returned as a `Buffer` object unless an encoding has been specified using
		the `readable.setEncoding()` method or the stream is operating in object mode.

		@see https://nodejs.org/api/stream.html#stream_readable_read_size
	**/
	function read(?size:Int):Null<Dynamic>;

	/**
		Is `true` if it is safe to call `readable.read()`.

		@see https://nodejs.org/api/stream.html#stream_readable_readable
	**/
	var readable(default, null):Bool;

	/**
		Getter for the property `encoding` of a given `Readable` stream.
		The `encoding` property can be set using the `readable.setEncoding()` method.

		@see https://nodejs.org/api/stream.html#stream_readable_readableencoding
	**/
	var readableEncoding(default, null):Null<String>;

	/**
		Becomes `true` when `'end'` event is emitted.

		@see https://nodejs.org/api/stream.html#stream_readable_readableended
	**/
	var readableEnded(default, null):Bool;

	/**
		Returns the value of `highWaterMark` passed when constructing this `Readable`.

		@see https://nodejs.org/api/stream.html#stream_readable_readablehighwatermark
	**/
	var readableHighWaterMark(default, null):Int;

	/**
		This property contains the number of bytes (or objects) in the queue ready to be read.
		The value provides introspection data regarding the status of the `highWaterMark`.

		@see https://nodejs.org/api/stream.html#stream_readable_readablelength
	**/
	var readableLength(default, null):Int;

	/**
		Getter for the property `objectMode` of a given `Readable` stream.

		@see https://nodejs.org/api/stream.html#stream_readable_readableobjectmode
	**/
	var readableObjectMode(default, null):Bool;

	/**
		The `readable.resume()` method causes an explicitly paused `Readable` stream to resume emitting `'data'` events,
		switching the stream into flowing mode.

		@see https://nodejs.org/api/stream.html#stream_readable_resume
	**/
	function resume():TSelf;

	/**
		The `readable.setEncoding()` method sets the character encoding for data read from the `Readable` stream.

		@see https://nodejs.org/api/stream.html#stream_readable_setencoding_encoding
	**/
	function setEncoding(encoding:String):TSelf;

	/**
		The `readable.unpipe()` method detaches a `Writable` stream previously attached using the `stream.pipe()` method.

		@see https://nodejs.org/api/stream.html#stream_readable_unpipe_destination
	**/
	function unpipe(?destination:IWritable):TSelf;

	/**
		Passing `chunk` as `null` signals the end of the stream (EOF), after which no more data can be written.

		@see https://nodejs.org/api/stream.html#stream_readable_unshift_chunk_encoding
	**/
	function unshift(chunk:Null<Dynamic>, ?encoding:String):Void;

	/**
		Prior to Node.js 0.10, streams did not implement the entire `stream` module API as it is currently defined.
		(See Compatibility for more information.)

		@see https://nodejs.org/api/stream.html#stream_readable_wrap_stream
	**/
	function wrap(stream:Dynamic):IReadable;

	// --------- API for implementing a Readable Stream -----------------------

	/**
		@see https://nodejs.org/api/stream.html#stream_new_stream_readable_options
	**/
	function new(?options:ReadableNewOptions);

	/**
		This function **MUST NOT** be called by application code directly.
		It should be implemented by child classes, and called by the internal `Readable` class methods only.

		@see https://nodejs.org/api/stream.html#stream_readable_read_size_1
	**/
	private function _read(size:Int):Void;

	/**
		The `_destroy()` method is called by `readable.destroy()`.
		It can be overridden by child classes but it **must not** be called directly.

		@see https://nodejs.org/api/stream.html#stream_readable_destroy_err_callback
	**/
	private function _destroy(err:Null<Error>, callback:Null<Error>->Void):Void;

	/**
		The `readable.push()` method is intended be called only by `Readable` implementers,
		and only from within the `readable._read()` method.

		@see https://nodejs.org/api/stream.html#stream_readable_push_chunk_encoding
	**/
	private function push(chunk:Null<Dynamic>, ?encoding:String):Bool;

	// --------- TTY module API  ----------------------------------------------

	/**
		Terminal read streams (i.e. process.stdin) have this property set to true.
		It is false for any other read streams.

		@see https://nodejs.org/api/tty.html#tty_readstream_istty
	**/
	var isTTY(default, null):Bool;

	// --------- static API  --------------------------------------------------
	// TODO @:overload(function<T>(iterable:AsyncIterator<T>, ?options:ReadableNewOptions):IReadable {})
	static function from<T>(iterable:Iterator<T>, ?options:ReadableNewOptions):IReadable;
}

/**
	Options for `Readable` private constructor.
	For stream implementors only, see node.js API documentation
**/
typedef ReadableNewOptions = {
	/**
		The maximum number of bytes to store in the internal buffer before ceasing to read from the underlying resource.
		Default: `16384` (16kb), or `16` for `objectMode` streams.
	**/
	@:optional var highWaterMark:Int;

	/**
		If specified, then buffers will be decoded to strings using the specified encoding.
		Default: `null`.
	**/
	@:optional var encoding:String;

	/**
		Whether this stream should behave as a stream of objects.
		Meaning that `stream.read(n)` returns a single value instead of a `Buffer` of size `n`.
		Default: `false`.
	**/
	@:optional var objectMode:Bool;

	/**
		Whether or not the stream should emit `'close'` after it has been destroyed.
		Default: `true`.
	**/
	@:optional var emitClose:Bool;

	/**
		Implementation for the `stream._read()` method.
	**/
	#if haxe4
	@:optional var read:(size:Int) -> Void;
	#else
	@:optional var read:Int->Void;
	#end

	/**
		Implementation for the `stream._destroy()` method.
	**/
	#if haxe4
	@:optional var destroy:(err:Null<Error>, callback:Null<Error>->Void) -> Void;
	#else
	@:optional var destroy:Null<Error>->(Null<Error>->Void)->Void;
	#end

	/**
		Whether this stream should automatically call `.destroy()` on itself after ending.
		Default: `false`.
	**/
	@:optional var autoDestroy:Bool;
}

/**
	`IReadable` interface is used as "any Readable".

	See `Readable` for actual class documentation.
**/
@:remove
extern interface IReadable extends IStream {
	function destroy(?error:Error):IReadable;

	var destroyed(default, null):Bool;

	function isPaused():Bool;

	function pause():IReadable;

	function pipe<T:IWritable>(destination:T, ?options:{?end:Bool}):T;

	function read(?size:Int):Null<Dynamic>;

	var readable(default, null):Bool;

	var readableEncoding(default, null):Null<String>;

	var readableEnded(default, null):Bool;

	var readableHighWaterMark(default, null):Int;

	var readableLength(default, null):Int;

	var readableObjectMode(default, null):Bool;

	function resume():IReadable;

	function setEncoding(encoding:String):IReadable;

	function unpipe(?destination:IWritable):IReadable;

	function unshift(chunk:Null<Dynamic>, ?encoding:String):Void;

	function wrap(stream:Dynamic):IReadable;
}

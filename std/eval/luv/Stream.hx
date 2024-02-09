package eval.luv;

@:coreType abstract TStream<T> to Stream {}
// typedef TStream<T> = Stream;

enum SendHandle {
	TCP(tcp:Tcp);
	PIPE(pipe:Pipe);
}

/**
	Streams.

	@see https://aantron.github.io/luv/luv/Luv/Stream
**/
@:coreType abstract Stream to Handle {
	/**
		Shuts down the write side of the stream.
	**/
	extern static public function shutdown(stream:Stream, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Starts listening for incoming connections.

		`backlog` indicates the number of connections the kernel might queue.
		When a new incoming connection is received the `callback` is called.
	**/
	extern static public function listen(stream:Stream, callback:(result:Result<Result.NoData>)->Void, ?backlog:Int):Void;

	/**
		This call is used in conjunction with `Stream.listen()` to accept incoming
		connections. Call this function after receiving a `callback` of `listen(callback)`
		to accept the connection. Before calling this function the client handle
		must be initialized.

		When the `callback` of `listen(callback)` is called it is guaranteed that
		this function will complete successfully the first time.

		`client` should be a freshly-initialized stream.
	**/
	extern static public function accept<T>(server:TStream<T>, client:TStream<T>):Result<Result.NoData>;

	/**
		Calls the `callback` whenever data is available on the stream.

		The amount of data read is equal to the length of the buffer passed to
		the `callback`. `allocate` is called immediately before each call to the
		main `callback`, to create buffer, into which the data will be read.

		The end of the stream (typically, when the remote peer closes or shuts down
		the connection) is indicated by `UVError.UV_EOF` being passed to the `callback`.
		Note that this behavior is different from `eval.luv.File.read`.

		Zero-length reads are possible, and do not indicate the end of stream. Instead,
		they usually indicate `UVError.UV_EAGAIN` inside libuv; libuv still calls the
		`callback` in order to give the C user a chance to deallocate the data buffer.
		This is not usually an issue in OCaml (which is the backend for eval target of
		Haxe), so a wrapper of this function can usually simply ignore zero-length reads.
		It is then also safe to convert `UVError.UV_EOF` to zero-length reads in a
		higher-level API, for consistency with reading files, and in accordance with OS
		API convention.

		To read only once, call `eval.luv.Stream.readStop` immediately, in the `callback`.
		Otherwise, the main callback will be called repeatedly.
	**/
	extern static public function readStart(stream:Stream, callback:(result:Result<Buffer>)->Void, ?allocate:(size:Int)->Buffer):Void;

	/**
		Stops reading.
	**/
	extern static public function readStop(stream:Stream):Result<Result.NoData>;

	/**
		Writes the given buffer to the stream.

		The second argument passed to the `callback` is the number of bytes written.
		libuv has an internal queue of writes, in part to implement retry. This means
		that writes can be partial at the libuv API level, so it is possible to receive
		both an `UVError` result, and for some data to have been successfully written.
	**/
	extern static public function write(stream:Stream, data:Array<Buffer>, callback:(result:Result<Result.NoData>, bytesWritten:Int)->Void):Result<Result.NoData>;

	/**
		Like `eval.luv.Stream.write`, but allows sending a TCP socket or pipe over the
		stream.
	**/
	extern static public function write2(stream:TStream<Pipe>, data:Array<Buffer>, sendHandle:SendHandle, callback:(result:Result<Result.NoData>, bytesWritten:Int)->Void):Result<Result.NoData>;

	/**
		Same as `eval.luv.Stream.write()`, but won’t queue a write request if it can’t
		be completed immediately.

		Returns the number of bytes written.
	**/
	extern static public function tryWrite(stream:Stream, data:Array<Buffer>):Result<Int>;

	/**
		Indicates whether the stream is readable (has data).
	**/
	extern static public function isReadable(stream:Stream):Bool;

	/**
		Indicates whether the stream is writable (has space in buffers).
	**/
	extern static public function isWritable(stream:Stream):Bool;

	/**
		Sets the blocking mode of the stream.
	**/
	extern static public function setBlocking(stream:Stream, block:Bool):Result<Result.NoData>;
}
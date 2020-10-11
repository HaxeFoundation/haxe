package eval.luv;

import haxe.ds.Option;

enum abstract PipeMode(Int) {
	var READ = 0;
	var WRITE = 1;
	var READ_WRITE = 2;
}

enum ReceiveHandle {
	NONE;
	TCP(associate:(tcp:Tcp)->Result<Result.NoData>);
	PIPE(associate:(pipe:Pipe)->Result<Result.NoData>);
}

/**
	Pipes

	@see https://aantron.github.io/luv/luv/Luv/Pipe
**/
@:using(eval.luv.Handle)
@:using(eval.luv.Stream)
@:coreType abstract Pipe to Handle to Stream to Stream.TStream<Pipe> {
	/**
		Allocates and initializes a pipe.

		The pipe is not yet connected to anything at this point.
	**/
	static public function init(?loop:Loop, forHandlePassing:Bool = false):Result<Pipe>;

	/**
		Assigns a pipe a name or an address.
	**/
	public function bind(nameOrAddress:NativeString):Result<Result.NoData>;

	/**
		Connects to the pipe at the given name or address.
	**/
	public function connect(target:NativeString, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Retrieves the name or address assigned to the pipe.
	**/
	public function getSockName():Result<NativeString>;

	/**
		Retrieves the name or address of the pipe's peer.
	**/
	public function getPeerName():Result<NativeString>;

	/**
		Set the number of pending pipe instance handles when the pipe server is
		waiting for connections.
	**/
	public function pendingInstances(amount:Int):Void;

	/**
		Receives a file descriptor over the given pipe.

		File descriptors are sent using the `send_handle` argument of `eval.luv.Stream.write2`.
		On the receiving end, call `eval.luv.Stream.read_start`. When that function
		calls its callback, there may be file descriptors in the pipe, in addition
		to the ordinary data provided to the callback.

		To check, call this function `eval.luv.Pipe.recieve_handle` in a loop until
		it returns `None`. Each time it returns `TTcp` or `TPipe`, create an appropriate
		handle using either `eval.luv.TCP.init` or `eval.uv.Pipe.init`, and call
		`receive_handle` to receive the file descriptor and associate it with handle.
	**/
	public function receiveHandle():ReceiveHandle;

	/**
		Sets pipe permissions.
	**/
	public function chmod(mode:PipeMode):Result<Result.NoData>;
}
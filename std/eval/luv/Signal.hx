package eval.luv;

/**
	For the moment, the signals exposed are those that are both present on Unix
	and present or emulated by libuv on Windows.

	You can also provide a plain integer signal code instead of the values of
	this enum.

	@see https://aantron.github.io/luv/luv/Luv/Signal#signals
**/
extern enum abstract SigNum(Int) from Int to Int {
	var SIGABRT;
	var SIGFPE;
	var SIGHUP;
	var SIGILL;
	var SIGINT;
	var SIGKILL;
	var SIGSEGV;
	var SIGTERM;
	var SIGWINCH;
}

/**
	Signals.

	@see https://aantron.github.io/luv/luv/Luv/Signal
**/
@:using(eval.luv.Handle)
@:coreType abstract Signal to Handle {
	/**
		Allocates and initializes a signal handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<Signal>;

	/**
		Starts the signal handle.
	**/
	public function start(sigNum:SigNum, callback:()->Void):Result<Result.NoData>;

	/**
		Like `eval.luv.Signal.start`, but the handle is stopped after one callback call.
	**/
	public function startOneshot(sigNum:SigNum, callback:()->Void):Result<Result.NoData>;

	/**
		Stops the signal handle.
	**/
	public function stop():Result<Result.NoData>;

	/**
		Evaluates to the signal number associated with the handle.
	**/
	public function signum():Int;
}
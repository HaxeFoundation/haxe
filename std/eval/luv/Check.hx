package eval.luv;

/**
	Post-I/O callback.

	@see https://aantron.github.io/luv/luv/Luv/Check
**/
@:using(eval.luv.Handle)
@:coreType abstract Check to Handle {
	/**
		Allocate and initialize a check handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<Check>;

	/**
		Starts the handle with the given callback.
	**/
	public function start(callback:()->Void):Result<Result.NoData>;

	/**
		Stops the handle.
	**/
	public function stop():Result<Result.NoData>;
}
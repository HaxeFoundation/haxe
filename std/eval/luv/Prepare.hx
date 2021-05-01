package eval.luv;

/**
	Pre-I/O callback.

	@see https://aantron.github.io/luv/luv/Luv/Prepare
**/
@:using(eval.luv.Handle)
@:coreType abstract Prepare to Handle {
	/**
		Allocate and initialize a prepare handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<Prepare>;

	/**
		Starts the handle with the given callback.
	**/
	public function start(callback:()->Void):Result<Result.NoData>;

	/**
		Stops the handle.
	**/
	public function stop():Result<Result.NoData>;
}
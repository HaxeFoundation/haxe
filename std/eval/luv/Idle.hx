package eval.luv;

/**
	Per-iteration callback.

	@see https://aantron.github.io/luv/luv/Luv/Idle
**/
@:using(eval.luv.Handle)
@:coreType abstract Idle to Handle {
	/**
		Allocate and initialize an idle handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<Idle>;

	/**
		Starts the handle with the given callback.
	**/
	public function start(callback:()->Void):Result<Result.NoData>;

	/**
		Stops the handle.
	**/
	public function stop():Result<Result.NoData>;
}
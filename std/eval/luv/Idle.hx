package eval.luv;

@:coreType abstract IdleHandle to Handle {}

/**
	Per-iteration callback.

	@see https://aantron.github.io/luv/luv/Luv/Idle
**/
extern class Idle {
	/**
		Allocate and initialize an idle handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(?loop:Loop):Result<IdleHandle>;

	/**
		Starts the handle with the given callback.
	**/
	static public function start(idle:IdleHandle, callback:()->Void):Result<Result.NoData>;

	/**
		Stops the handle.
	**/
	static public function stop(idle:IdleHandle):Result<Result.NoData>;
}
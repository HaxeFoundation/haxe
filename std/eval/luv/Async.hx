package eval.luv;

/**
	Inter-loop communication.

	@see https://aantron.github.io/luv/luv/Luv/Async
**/
@:using(eval.luv.Handle)
@:coreType abstract Async to Handle {
	/**
		Allocates and initializes an async handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(callback:(async:Async)->Void, ?loop:Loop):Result<Async>;

	/**
		Triggers a call to the handle's callback by the handle's loop.
	**/
	public function send():Result<Result.NoData>;
}
package eval.luv;

/**
	Timers.

	@see https://aantron.github.io/luv/luv/Luv/Timer
**/
@:using(eval.luv.Handle)
@:coreType abstract Timer to Handle {
	/** The timer repeat interval. */
	public var repeat(get,set):Int;
	function get_repeat():Int;
	function set_repeat(v:Int):Int;

	/** Evaluates to the time until the timer expires, or zero if it has already expired. */
	public var dueIn(get,never):Int;
	function get_dueIn():Int;

	/**
		Allocate and initialize an idle handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<Timer>;

	/**
		Starts a timer.
	**/
	public function start(callback:()->Void, timeoutMs:Int, ?repeatMs:Int):Result<Result.NoData>;

	/**
		Stops a timer.
	**/
	public function stop():Result<Result.NoData>;

	/**
		Restarts a timer.
	**/
	public function again():Result<Result.NoData>;
}
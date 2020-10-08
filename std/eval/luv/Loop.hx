package eval.luv;

enum abstract RunMode(Int) {
	/** Runs the event loop until there are no more active and referenced handles or requests. */
	var DEFAULT = 0;
	/** Poll for i/o once. Note that this mode blocks if there are no pending callbacks. */
	var ONCE = 1;
	/** Poll for i/o once but don't block if there are no pending callbacks. */
	var NOWAIT = 2;
}

/**
	Event loops.

	@see https://aantron.github.io/luv/luv/Luv/Loop
**/
@:coreType abstract Loop {
	/**
		Runs an event loop.

		If `loop` is specified, the given loop will be run.
		If not, this function will run `Loop.defaultLoop`.
	**/
	extern static public function run(?loop:Loop, mode:RunMode):Bool;

	/**
		Stops an event loop as soon as possible.
	**/
	extern static public function stop(loop:Loop):Void;

	/**
		Returns the default event loop.
	**/
	extern static public function defaultLoop():Loop;

	/**
		Allocates and initializes a new event loop.
	**/
	extern static public function init():Result<Loop>;

	/**
		Releases resources associated with an event loop.
	**/
	extern static public function close(loop:Loop):Result<Result.NoData>;

	/**
		Indicates whether the loop is monitoring any activity.
	**/
	extern static public function alive(loop:Loop):Bool;
}
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
	Configuration options.
	@see http://docs.libuv.org/en/v1.x/loop.html#c.uv_loop_configure
**/
enum abstract LoopOption<T>(Int) {
	extern static public final sigprof:Int;

	var LOOP_BLOCK_SIGNAL:LoopOption<Int> = 0;
	var METRICS_IDLE_TIME:LoopOption<Result.NoData> = 1;
}

/**
	Event loops.

	@see https://aantron.github.io/luv/luv/Luv/Loop

	Haxe event loops define an implicit cast to libuv loops. That is, you can use
	`sys.thread.Thread.current().events` in any place where `eval.luv.Loop` is
	expected.
**/
@:coreType abstract Loop {
	@:from
	static inline function fromHaxeEventLoop(events:sys.thread.EventLoop):Loop {
		return events.handle;
	}

	/**
		Returns the default event loop.
	**/
	static public function defaultLoop():Loop;

	/**
		Allocates and initializes a new event loop.
	**/
	static public function init():Result<Loop>;

	/**
		Releases any state libuv is holding on to.

		Normally there's no need to do this manually.

		Warning! Only call `Loop.libraryShutdown()` once.
		Warning! Don’t call `Loop.libraryShutdown()` when there are still event loops or I/O requests active.
		Warning! Don’t call libuv functions after calling `Loop.libraryShutdown()`.
	**/
	static public function libraryShutdown():Void;

	/**
		Runs an event loop.
	**/
	public function run(mode:RunMode):Bool;

	/**
		Releases resources associated with an event loop.
	**/
	public function close():Result<Result.NoData>;

	/**
		Indicates whether the loop is monitoring any activity.
	**/
	public function alive():Bool;

	/**
		Stops an event loop as soon as possible.
	**/
	public function stop():Void;

	/**
		Returns the cached loop timestamp.
	**/
	public function now():eval.integers.UInt64;

	/**
		Updates the cached loop timestamp.
	**/
	public function updateTime():Void;

	/**
		Sets the loop option.
	**/
	public function configure<T>(option:LoopOption<T>, value:T):Result<Result.NoData>;
}
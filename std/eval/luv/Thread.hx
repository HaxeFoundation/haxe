package eval.luv;

/**
	Threads.

	@see https://aantron.github.io/luv/luv/Luv/Thread

	`eval.luv` integrates libuv with the OCaml runtime lock. This means that, as
	in any other OCaml program, two threads cannot be running OCaml code at the
	same time. Thus, two threads cannot be running Haxe code at the same time
	because eval interpreter is written in OCaml.
	However, `eval.luv` releases the lock when calling a potentially-blocking libuv API,
	so that other threads can run while the calling thread is blocked. In particular,
	the lock is released during calls to `eval.luv.Loop.run`, which means that other
	threads can run in between when you make a call to a non-blocking API, and when
	its callback is called by libuv.
**/
@:coreType abstract Thread {
	/**
		Returns the representation of the calling thread.
	**/
	static public function self():Thread;

	/**
		Starts a new thread, which will run the given function.
	**/
	static public function create(fn:()->Void, ?stackSize:Int):Result<Thread>;

	/**
		Waits for the thread to terminate.
	**/
	public function join():Result<Result.NoData>;

}
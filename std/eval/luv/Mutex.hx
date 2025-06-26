package eval.luv;

/**
	Mutexes.

	@see https://aantron.github.io/luv/luv/Luv/Mutex
**/
@:coreType abstract Mutex {
	/**
		Allocates and initializes a mutex.
	**/
	static public function init(?recursive:Bool):Result<Mutex>;

	/**
		Cleans up a mutex.
	**/
	public function destroy():Void;

	/**
		Takes the mutex.

		The calling thread is blocked until it obtains the mutex.
	**/
	public function lock():Void;

	/**
		Tries to take the mutex without blocking.
	**/
	public function tryLock():Result<Result.NoData>;

	/**
		Releases the mutex.
	**/
	public function unlock():Void;
}
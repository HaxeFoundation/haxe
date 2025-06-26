package eval.luv;

/**
	Semaphores.

	@see https://aantron.github.io/luv/luv/Luv/Semaphore
**/
@:coreType abstract Semaphore {
	/**
		Allocates and initializes a read-write lock.
	**/
	static public function init(value:Int):Result<Semaphore>;

	/**
		Cleans up a semaphore.
	**/
	public function destroy():Void;

	/**
		Increments a semaphore.
	**/
	public function post():Void;

	/**
		Decrements a semaphore.
	**/
	public function wait():Void;

	/**
		Tries to decrement a semaphore without blocking.
	**/
	public function tryWait():Result<Result.NoData>;
}
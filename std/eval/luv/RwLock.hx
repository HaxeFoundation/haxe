package eval.luv;

/**
	Read-write locks.

	@see https://aantron.github.io/luv/luv/Luv/Rwlock
**/
@:coreType abstract RwLock {
	/**
		Allocates and initializes a read-write lock.
	**/
	static public function init():Result<RwLock>;

	/**
		Cleans up a read-write lock.
	**/
	public function destroy():Void;

	/**
		Takes a read-write lock for reading (shared access).
	**/
	public function rdLock():Void;

	/**
		Tries to take a read-write lock for reading without blocking.
	**/
	public function rdTryLock():Result<Result.NoData>;

	/**
		Releases a read-write lock after it was taken for reading.
	**/
	public function rdUnlock():Void;

	/**
		Takes a read-write lock for writing (exclusive access).
	**/
	public function wrLock():Void;

	/**
		Tries to take a read-write lock for writing without blocking.
	**/
	public function wrTryLock():Result<Result.NoData>;

	/**
		Releases a read-write lock after it was taken for writing.
	**/
	public function wrUnlock():Void;
}
package sys.thread;

#if (!target.threaded)
#error "This class is not available on this target"
#end
@:coreApi extern class Semaphore {
	/**
		Creates a new semaphore with an initial value.
	**/
	public function new(value:Int):Void;

	/**
		Locks the semaphore.
		If the value of the semaphore is zero, then the thread will block until it is able to lock the semaphore.
		If the value is non-zero, it is decreased by one.
	**/
	public function acquire():Void;

	/**
		Try to lock the semaphore.
		If the value of the semaphore is zero, `false` is returned, else the value is increased.

		If `timeout` is specified, this function will block until the thread is able to acquire the semaphore, or the timout expires.
		`timeout` is in seconds.
	**/
	public function tryAcquire(?timeout:Float):Bool;

	/**
		Release the semaphore.
		The value of the semaphore is increased by one.
	**/
	public function release():Void;
}

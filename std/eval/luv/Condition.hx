package eval.luv;

/**
	Condition variables.

	@see https://aantron.github.io/luv/luv/Luv/Condition
**/
@:coreType abstract Condition {
	/**
		Allocates and initializes a condition variable.
	**/
	static public function init():Result<Condition>;

	/**
		Cleans up a condition variable.
	**/
	public function destroy():Void;

	/**
		Signals a condition variable.
	**/
	public function signal():Void;

	/**
		Signals a condition variable, waking all waiters.
	**/
	public function broadcast():Void;

	/**
		Waits on a condition variable.
	**/
	public function wait(mutex:Mutex):Void;

	/**
		Waits on a condition variable with a timeout.
		The timeout is given in nanoseconds.
	**/
	public function timedWait(mutex:Mutex, timeout:Int):Void;
}
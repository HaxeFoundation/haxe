package haxe.async;

class Defer {
	/**
		Schedules the given function to run during the next processing tick.
		Convenience shortcut for `Timer.delay(f, 0)`.
	**/
	public static inline function nextTick(f:() -> Void):haxe.Timer {
		return haxe.Timer.delay(f, 0);
	}
}

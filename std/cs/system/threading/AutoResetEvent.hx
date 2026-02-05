package cs.system.threading;

/** Represents a thread synchronization event that, when signaled, resets automatically after releasing a single waiting thread. This class cannot be inherited. */
@:native("System.Threading.AutoResetEvent")
extern class AutoResetEvent extends cs.system.threading.EventWaitHandle {
	function new(initialState:Bool):Void;
}

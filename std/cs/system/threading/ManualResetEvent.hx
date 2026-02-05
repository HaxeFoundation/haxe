package cs.system.threading;

/** Represents a thread synchronization event that, when signaled, must be reset manually. This class cannot be inherited. */
@:native("System.Threading.ManualResetEvent")
extern class ManualResetEvent extends cs.system.threading.EventWaitHandle {
	function new(initialState:Bool):Void;
}

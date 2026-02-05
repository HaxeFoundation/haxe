package cs.system.threading;

/** Represents pre-allocated state for native overlapped I/O operations. */
@:native("System.Threading.PreAllocatedOverlapped")
extern class PreAllocatedOverlapped {
	function new(callback:cs.system.threading.IOCompletionCallback, state:Dynamic, pinData:Dynamic):Void;
	/** Frees the resources associated with this  instance. */
	function Dispose():Void;
}

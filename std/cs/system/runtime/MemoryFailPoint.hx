package cs.system.runtime;

/** Checks for sufficient memory resources before executing an operation. This class cannot be inherited. */
@:native("System.Runtime.MemoryFailPoint")
extern class MemoryFailPoint extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	function new(sizeInMegabytes:Int):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
}

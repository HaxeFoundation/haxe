package cs.system;

/** Represents a multicast delegate; that is, a delegate that can have more than one element in its invocation list. */
@:native("System.MulticastDelegate")
extern class MulticastDelegate {
	function new(func:Dynamic):Void;
}

package cs.system.threading;

/**
 * Represents a method to be called when a message is to be dispatched to a
 * synchronization context.
 * @param state The object passed to the delegate.
 */
@:native("System.Threading.SendOrPostCallback")
extern class SendOrPostCallback extends cs.system.MulticastDelegate {
	function new(func:(state:Dynamic)->Void):Void;
	function Invoke(state:Dynamic):Void;
}

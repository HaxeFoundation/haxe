package cs.system.threading;

/**
 * Represents a method to be called within a new context.
 * @param state An object containing information to be used by the callback method
 * each time it executes.
 */
@:native("System.Threading.ContextCallback")
extern class ContextCallback extends cs.system.MulticastDelegate {
	function new(func:(state:Dynamic)->Void):Void;
	function Invoke(state:Dynamic):Void;
}

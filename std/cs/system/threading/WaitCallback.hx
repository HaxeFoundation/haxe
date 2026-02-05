package cs.system.threading;

/**
 * Represents a callback method to be executed by a thread pool thread.
 * @param state An object containing information to be used by the callback method.
 */
@:native("System.Threading.WaitCallback")
extern class WaitCallback extends cs.system.MulticastDelegate {
	function new(func:(state:Dynamic)->Void):Void;
	function Invoke(state:Dynamic):Void;
}

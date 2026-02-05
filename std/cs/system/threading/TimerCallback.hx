package cs.system.threading;

/**
 * Represents the method that handles calls from a .
 * @param state An object containing application-specific information relevant to
 * the method invoked by this delegate, or .
 */
@:native("System.Threading.TimerCallback")
extern class TimerCallback extends cs.system.MulticastDelegate {
	function new(func:(state:Dynamic)->Void):Void;
	function Invoke(state:Dynamic):Void;
}

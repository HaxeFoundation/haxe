package cs.system.threading;

/**
 * Represents a method to be called when a  is signaled or times out.
 * @param state An object containing information to be used by the callback method
 * each time it executes.
 * @param timedOut if the  timed out;  if it was signaled.
 */
@:native("System.Threading.WaitOrTimerCallback")
extern class WaitOrTimerCallback extends cs.system.MulticastDelegate {
	function new(func:(state:Dynamic, timedOut:Bool)->Void):Void;
	function Invoke(state:Dynamic, timedOut:Bool):Void;
}

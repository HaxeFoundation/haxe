package cs.system.threading;

/**
 * Represents the method that executes on a .
 * @param obj An object that contains data for the thread procedure.
 */
@:native("System.Threading.ParameterizedThreadStart")
extern class ParameterizedThreadStart extends cs.system.MulticastDelegate {
	function new(func:(obj:Dynamic)->Void):Void;
	function Invoke(obj:Dynamic):Void;
}

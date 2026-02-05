package cs.system;

/**
 * Represents the method that handles the  event of an .
 * @param sender The source of the event.
 * @param args An  that contains the event data.
 */
@:native("System.AssemblyLoadEventHandler")
extern class AssemblyLoadEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, args:cs.system.AssemblyLoadEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, args:cs.system.AssemblyLoadEventArgs):Void;
}

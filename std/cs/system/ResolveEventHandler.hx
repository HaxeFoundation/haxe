package cs.system;

/**
 * Represents a method that handles the , , or  event of an .
 * @param sender The source of the event.
 * @param args The event data.
 * @return The assembly that resolves the type, assembly, or resource; or  if the
 * assembly cannot be resolved.
 */
@:native("System.ResolveEventHandler")
extern class ResolveEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, args:cs.system.ResolveEventArgs)->cs.system.reflection.Assembly):Void;
	function Invoke(sender:Dynamic, args:cs.system.ResolveEventArgs):cs.system.reflection.Assembly;
}

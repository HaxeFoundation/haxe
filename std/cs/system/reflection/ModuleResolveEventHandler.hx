package cs.system.reflection;

/**
 * Represents the method that will handle the  event of an .
 * @param sender The assembly that was the source of the event.
 * @param e The arguments supplied by the object describing the event.
 * @return The module that satisfies the request.
 */
@:native("System.Reflection.ModuleResolveEventHandler")
extern class ModuleResolveEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.ResolveEventArgs)->cs.system.reflection.Module):Void;
	function Invoke(sender:Dynamic, e:cs.system.ResolveEventArgs):cs.system.reflection.Module;
}

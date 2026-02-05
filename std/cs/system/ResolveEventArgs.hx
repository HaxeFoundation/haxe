package cs.system;

/** Provides data for loader resolution events, such as the , , , and  events. */
@:native("System.ResolveEventArgs")
extern class ResolveEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the name of the item to resolve.
	 * @return The name of the item to resolve.
	 */
	var Name(default, never):String;
	/**
	 * Gets the assembly whose dependency is being resolved.
	 * @return The assembly that requested the item specified by the  property.
	 */
	var RequestingAssembly(default, never):cs.system.reflection.Assembly;
	@:overload(function(name:String):Void {})
	function new(name:String, requestingAssembly:cs.system.reflection.Assembly):Void;
}

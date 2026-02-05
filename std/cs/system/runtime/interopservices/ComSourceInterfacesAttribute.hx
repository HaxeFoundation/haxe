package cs.system.runtime.interopservices;

/** Identifies a list of interfaces that are exposed as COM event sources for the attributed class. */
@:native("System.Runtime.InteropServices.ComSourceInterfacesAttribute")
extern class ComSourceInterfacesAttribute extends cs.system.Attribute {
	/**
	 * Gets the fully qualified name of the event source interface.
	 * @return The fully qualified name of the event source interface.
	 */
	var Value(default, never):String;
	@:overload(function(sourceInterfaces:String):Void {})
	@:overload(function(sourceInterface:cs.system.Type):Void {})
	@:overload(function(sourceInterface1:cs.system.Type, sourceInterface2:cs.system.Type):Void {})
	@:overload(function(sourceInterface1:cs.system.Type, sourceInterface2:cs.system.Type, sourceInterface3:cs.system.Type):Void {})
	function new(sourceInterface1:cs.system.Type, sourceInterface2:cs.system.Type, sourceInterface3:cs.system.Type, sourceInterface4:cs.system.Type):Void;
}

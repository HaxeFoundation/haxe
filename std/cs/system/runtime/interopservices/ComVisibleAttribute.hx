package cs.system.runtime.interopservices;

/** Controls accessibility of an individual managed type or member, or of all types within an assembly, to COM. */
@:native("System.Runtime.InteropServices.ComVisibleAttribute")
extern class ComVisibleAttribute extends cs.system.Attribute {
	/**
	 * Gets a value that indicates whether the COM type is visible.
	 * @return if the type is visible; otherwise, . The default value is .
	 */
	var Value(default, never):Bool;
	function new(visibility:Bool):Void;
}

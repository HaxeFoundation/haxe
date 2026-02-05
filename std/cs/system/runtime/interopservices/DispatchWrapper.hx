package cs.system.runtime.interopservices;

/** Wraps objects the marshaler should marshal as a . */
@:native("System.Runtime.InteropServices.DispatchWrapper")
extern class DispatchWrapper {
	/**
	 * Gets the object wrapped by the .
	 * @return The object wrapped by the .
	 */
	var WrappedObject(default, never):Dynamic;
	function new(obj:Dynamic):Void;
}

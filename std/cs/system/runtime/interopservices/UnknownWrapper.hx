package cs.system.runtime.interopservices;

/** Wraps objects the marshaler should marshal as a . */
@:native("System.Runtime.InteropServices.UnknownWrapper")
extern class UnknownWrapper {
	/**
	 * Gets the object contained by this wrapper.
	 * @return The wrapped object.
	 */
	var WrappedObject(default, never):Dynamic;
	function new(obj:Dynamic):Void;
}

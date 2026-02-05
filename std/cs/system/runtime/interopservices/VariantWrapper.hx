package cs.system.runtime.interopservices;

/** Marshals data of type  from managed to unmanaged code. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.VariantWrapper")
extern class VariantWrapper {
	/**
	 * Gets the object wrapped by the  object.
	 * @return The object wrapped by the  object.
	 */
	var WrappedObject(default, never):Dynamic;
	function new(obj:Dynamic):Void;
}

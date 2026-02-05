package cs.system.runtime.interopservices;

/** Marshals data of type  from managed to unmanaged code. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.BStrWrapper")
extern class BStrWrapper {
	/**
	 * Gets the wrapped  object to marshal as type .
	 * @return The object that is wrapped by .
	 */
	var WrappedObject(default, never):String;
	@:overload(function(value:Dynamic):Void {})
	function new(value:String):Void;
}

package cs.system.runtime.interopservices;

/** Provides a way for clients to access the actual object, rather than the adapter object handed out by a custom marshaler. */
@:native("System.Runtime.InteropServices.ICustomAdapter")
extern interface ICustomAdapter {
	/**
	 * Provides access to the underlying object wrapped by a custom marshaler.
	 * @return The object contained by the adapter object.
	 */
	function GetUnderlyingObject():Dynamic;
}

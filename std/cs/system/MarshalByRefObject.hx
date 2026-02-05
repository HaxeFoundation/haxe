package cs.system;

/** Enables access to objects across application domain boundaries in applications that support remoting. */
@:native("System.MarshalByRefObject")
extern class MarshalByRefObject {
	/**
	 * Retrieves the current lifetime service object that controls the lifetime policy
	 * for this instance.
	 * @return An object of type  used to control the lifetime policy for this
	 * instance.
	 */
	function GetLifetimeService():Dynamic;
	/**
	 * Obtains a lifetime service object to control the lifetime policy for this
	 * instance.
	 * @return An object of type  used to control the lifetime policy for this
	 * instance. This is the current lifetime service object for this instance if one
	 * exists; otherwise, a new lifetime service object initialized to the value of the
	 * property.
	 */
	function InitializeLifetimeService():Dynamic;
}

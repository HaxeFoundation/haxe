package cs.system;

/** Defines a mechanism for retrieving a service object; that is, an object that provides custom support to other objects. */
@:native("System.IServiceProvider")
extern interface IServiceProvider {
	/**
	 * Gets the service object of the specified type.
	 * @param serviceType An object that specifies the type of service object to get.
	 * @return A service object of type . -or- if there is no service object of type .
	 */
	function GetService(serviceType:cs.system.Type):Dynamic;
}

package cs.system.componentmodel.design;

/** Provides a simple implementation of the  interface. This class cannot be inherited. */
@:native("System.ComponentModel.Design.ServiceContainer")
extern class ServiceContainer {
	/**
	 * Gets the default services implemented directly by .
	 * @return The default services.
	 */
	var DefaultServices(default, never):cs.NativeArray<cs.system.Type>;
	@:overload(function():Void {})
	function new(parentProvider:cs.system.IServiceProvider):Void;
	@:overload(function(serviceType:cs.system.Type, callback:cs.system.componentmodel.design.ServiceCreatorCallback):Void {})
	@:overload(function(serviceType:cs.system.Type, serviceInstance:Dynamic):Void {})
	@:overload(function(serviceType:cs.system.Type, callback:cs.system.componentmodel.design.ServiceCreatorCallback, promote:Bool):Void {})
	/**
	 * Adds the specified service to the service container.
	 * @param serviceType The type of service to add.
	 * @param callback A callback object that can create the service. This allows a
	 * service to be declared as available, but delays creation of the object until the
	 * service is requested.
	 */
	function AddService(serviceType:cs.system.Type, serviceInstance:Dynamic, promote:Bool):Void;
	/** Disposes this service container. */
	function Dispose():Void;
	/**
	 * Gets the requested service.
	 * @param serviceType The type of service to retrieve.
	 * @return An instance of the service if it could be found, or  if it could not be
	 * found.
	 */
	function GetService(serviceType:cs.system.Type):Dynamic;
	@:overload(function(serviceType:cs.system.Type):Void {})
	/**
	 * Removes the specified service type from the service container.
	 * @param serviceType The type of service to remove.
	 */
	function RemoveService(serviceType:cs.system.Type, promote:Bool):Void;
}

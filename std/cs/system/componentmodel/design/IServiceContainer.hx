package cs.system.componentmodel.design;

/** Provides a container for services. */
@:native("System.ComponentModel.Design.IServiceContainer")
extern interface IServiceContainer extends cs.system.IServiceProvider {
	@:overload(function(serviceType:cs.system.Type, callback:cs.system.componentmodel.design.ServiceCreatorCallback):Void {})
	@:overload(function(serviceType:cs.system.Type, serviceInstance:Dynamic):Void {})
	@:overload(function(serviceType:cs.system.Type, callback:cs.system.componentmodel.design.ServiceCreatorCallback, promote:Bool):Void {})
	/**
	 * Adds the specified service to the service container.
	 * @param serviceType The type of service to add.
	 * @param callback A callback object that is used to create the service. This
	 * allows a service to be declared as available, but delays the creation of the
	 * object until the service is requested.
	 */
	function AddService(serviceType:cs.system.Type, serviceInstance:Dynamic, promote:Bool):Void;
	@:overload(function(serviceType:cs.system.Type):Void {})
	/**
	 * Removes the specified service type from the service container.
	 * @param serviceType The type of service to remove.
	 */
	function RemoveService(serviceType:cs.system.Type, promote:Bool):Void;
}

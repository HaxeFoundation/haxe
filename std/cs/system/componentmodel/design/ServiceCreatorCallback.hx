package cs.system.componentmodel.design;

/**
 * Provides a callback mechanism that can create an instance of a service on
 * demand.
 * @param container The service container that requested the creation of the
 * service.
 * @param serviceType The type of service to create.
 * @return The service specified by , or  if the service could not be created.
 */
@:native("System.ComponentModel.Design.ServiceCreatorCallback")
extern class ServiceCreatorCallback extends cs.system.MulticastDelegate {
	function new(func:(container:cs.system.componentmodel.design.IServiceContainer, serviceType:cs.system.Type)->Dynamic):Void;
	function Invoke(container:cs.system.componentmodel.design.IServiceContainer, serviceType:cs.system.Type):Dynamic;
}

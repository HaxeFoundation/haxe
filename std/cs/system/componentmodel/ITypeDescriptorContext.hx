package cs.system.componentmodel;

/** Provides contextual information about a component, such as its container and property descriptor. */
@:native("System.ComponentModel.ITypeDescriptorContext")
extern interface ITypeDescriptorContext extends cs.system.IServiceProvider {
	/**
	 * Gets the container representing this  request.
	 * @return An  with the set of objects for this ; otherwise,  if there is no
	 * container or if the  does not use outside objects.
	 */
	var Container(default, never):cs.system.componentmodel.IContainer;
	/**
	 * Gets the object that is connected with this type descriptor request.
	 * @return The object that invokes the method on the ; otherwise,  if there is no
	 * object responsible for the call.
	 */
	var Instance(default, never):Dynamic;
	/**
	 * Gets the  that is associated with the given context item.
	 * @return The  that describes the given context item; otherwise,  if there is no 
	 * responsible for the call.
	 */
	var PropertyDescriptor(default, never):cs.system.componentmodel.PropertyDescriptor;
	/** Raises the  event. */
	function OnComponentChanged():Void;
	/**
	 * Raises the  event.
	 * @return if this object can be changed; otherwise, .
	 */
	function OnComponentChanging():Bool;
}

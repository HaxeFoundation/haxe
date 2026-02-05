package cs.system.componentmodel.design;

/** Provides a service for registering event handlers for component events. */
@:native("System.ComponentModel.Design.IEventBindingService")
extern interface IEventBindingService {
	/**
	 * Creates a unique name for an event-handler method for the specified component
	 * and event.
	 * @param component The component instance the event is connected to.
	 * @param e The event to create a name for.
	 * @return The recommended name for the event-handler method for this event.
	 */
	function CreateUniqueMethodName(component:cs.system.componentmodel.IComponent, e:cs.system.componentmodel.EventDescriptor):String;
	/**
	 * Gets a collection of event-handler methods that have a method signature
	 * compatible with the specified event.
	 * @param e The event to get the compatible event-handler methods for.
	 * @return A collection of strings.
	 */
	function GetCompatibleMethods(e:cs.system.componentmodel.EventDescriptor):cs.system.collections.ICollection;
	/**
	 * Gets an  for the event that the specified property descriptor represents, if it
	 * represents an event.
	 * @param property The property that represents an event.
	 * @return An  for the event that the property represents, or  if the property does
	 * not represent an event.
	 */
	function GetEvent(property:cs.system.componentmodel.PropertyDescriptor):cs.system.componentmodel.EventDescriptor;
	/**
	 * Converts a set of event descriptors to a set of property descriptors.
	 * @param events The events to convert to properties.
	 * @return An array of  objects that describe the event set.
	 */
	function GetEventProperties(events:cs.system.componentmodel.EventDescriptorCollection):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Converts a single event descriptor to a property descriptor.
	 * @param e The event to convert.
	 * @return A  that describes the event.
	 */
	function GetEventProperty(e:cs.system.componentmodel.EventDescriptor):cs.system.componentmodel.PropertyDescriptor;
	@:overload(function():Bool {})
	@:overload(function(lineNumber:Int):Bool {})
	/**
	 * Displays the user code for the designer.
	 * @return if the code is displayed; otherwise, .
	 */
	function ShowCode(component:cs.system.componentmodel.IComponent, e:cs.system.componentmodel.EventDescriptor):Bool;
}

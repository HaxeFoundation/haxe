package cs.system.componentmodel.design;

/** Provides an interface to modify the set of member descriptors for a component in design mode. */
@:native("System.ComponentModel.Design.ITypeDescriptorFilterService")
extern interface ITypeDescriptorFilterService {
	/**
	 * Filters the attributes that a component exposes through a .
	 * @param component The component to filter the attributes of.
	 * @param attributes A dictionary of attributes that can be modified.
	 * @return if the set of filtered attributes is to be cached;  if the filter
	 * service must query again.
	 */
	function FilterAttributes(component:cs.system.componentmodel.IComponent, attributes:cs.system.collections.IDictionary):Bool;
	/**
	 * Filters the events that a component exposes through a .
	 * @param component The component to filter events for.
	 * @param events A dictionary of events that can be modified.
	 * @return if the set of filtered events is to be cached;  if the filter service
	 * must query again.
	 */
	function FilterEvents(component:cs.system.componentmodel.IComponent, events:cs.system.collections.IDictionary):Bool;
	/**
	 * Filters the properties that a component exposes through a .
	 * @param component The component to filter properties for.
	 * @param properties A dictionary of properties that can be modified.
	 * @return if the set of filtered properties is to be cached;  if the filter
	 * service must query again.
	 */
	function FilterProperties(component:cs.system.componentmodel.IComponent, properties:cs.system.collections.IDictionary):Bool;
}

package cs.system.componentmodel.design;

/** Provides an interface that enables a designer to access and filter the dictionaries of a  that stores the property, attribute, and event descriptors that a component designer can expose to the design-time environment. */
@:native("System.ComponentModel.Design.IDesignerFilter")
extern interface IDesignerFilter {
	/**
	 * When overridden in a derived class, allows a designer to change or remove items
	 * from the set of attributes that it exposes through a .
	 * @param attributes The  objects for the class of the component. The keys in the
	 * dictionary of attributes are the  values of the attributes.
	 */
	function PostFilterAttributes(attributes:cs.system.collections.IDictionary):Void;
	/**
	 * When overridden in a derived class, allows a designer to change or remove items
	 * from the set of events that it exposes through a .
	 * @param events The  objects that represent the events of the class of the
	 * component. The keys in the dictionary of events are event names.
	 */
	function PostFilterEvents(events:cs.system.collections.IDictionary):Void;
	/**
	 * When overridden in a derived class, allows a designer to change or remove items
	 * from the set of properties that it exposes through a .
	 * @param properties The  objects that represent the properties of the class of the
	 * component. The keys in the dictionary of properties are property names.
	 */
	function PostFilterProperties(properties:cs.system.collections.IDictionary):Void;
	/**
	 * When overridden in a derived class, allows a designer to add items to the set of
	 * attributes that it exposes through a .
	 * @param attributes The  objects for the class of the component. The keys in the
	 * dictionary of attributes are the  values of the attributes.
	 */
	function PreFilterAttributes(attributes:cs.system.collections.IDictionary):Void;
	/**
	 * When overridden in a derived class, allows a designer to add items to the set of
	 * events that it exposes through a .
	 * @param events The  objects that represent the events of the class of the
	 * component. The keys in the dictionary of events are event names.
	 */
	function PreFilterEvents(events:cs.system.collections.IDictionary):Void;
	/**
	 * When overridden in a derived class, allows a designer to add items to the set of
	 * properties that it exposes through a .
	 * @param properties The  objects that represent the properties of the class of the
	 * component. The keys in the dictionary of properties are property names.
	 */
	function PreFilterProperties(properties:cs.system.collections.IDictionary):Void;
}

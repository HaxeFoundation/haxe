package cs.system.componentmodel.design;

/** Provides methods for identifying the components of a component. */
@:native("System.ComponentModel.Design.IInheritanceService")
extern interface IInheritanceService {
	/**
	 * Searches the specified component for fields that implement the  interface and
	 * adds each to the specified container, storing the inheritance level of each
	 * which can be retrieved using the  method.
	 * @param component The  to search. Searching begins with this component.
	 * @param container The  to add components to.
	 */
	function AddInheritedComponents(component:cs.system.componentmodel.IComponent, container:cs.system.componentmodel.IContainer):Void;
	/**
	 * Gets the inheritance attribute for the specified component.
	 * @param component The  for which to retrieve the inheritance attribute.
	 * @return An instance of  that describes the level of inheritance of the specified
	 * component.
	 */
	function GetInheritanceAttribute(component:cs.system.componentmodel.IComponent):cs.system.componentmodel.InheritanceAttribute;
}

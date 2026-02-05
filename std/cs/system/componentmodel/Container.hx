package cs.system.componentmodel;

/** Encapsulates zero or more components. */
@:native("System.ComponentModel.Container")
extern class Container {
	/**
	 * Gets all the components in the .
	 * @return A collection that contains the components in the .
	 */
	var Components(default, never):cs.system.componentmodel.ComponentCollection;
	function new():Void;
	@:overload(function(component:cs.system.componentmodel.IComponent):Void {})
	/**
	 * Adds the specified  to the . The component is unnamed.
	 * @param component The component to add.
	 */
	function Add(component:cs.system.componentmodel.IComponent, name:String):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Removes a component from the .
	 * @param component The component to remove.
	 */
	function Remove(component:cs.system.componentmodel.IComponent):Void;
}

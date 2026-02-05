package cs.system.componentmodel;

/** Provides functionality for containers. Containers are objects that logically contain zero or more components. */
@:native("System.ComponentModel.IContainer")
extern interface IContainer extends cs.system.IDisposable {
	/**
	 * Gets all the components in the .
	 * @return A collection of  objects that represents all the components in the .
	 */
	var Components(default, never):cs.system.componentmodel.ComponentCollection;
	@:overload(function(component:cs.system.componentmodel.IComponent):Void {})
	/**
	 * Adds the specified  to the  at the end of the list.
	 * @param component The  to add.
	 */
	function Add(component:cs.system.componentmodel.IComponent, name:String):Void;
	/**
	 * Removes a component from the .
	 * @param component The  to remove.
	 */
	function Remove(component:cs.system.componentmodel.IComponent):Void;
}

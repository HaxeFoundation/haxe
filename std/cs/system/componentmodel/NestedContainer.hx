package cs.system.componentmodel;

/** Provides the base implementation for the  interface, which enables containers to have an owning component. */
@:native("System.ComponentModel.NestedContainer")
extern class NestedContainer extends cs.system.componentmodel.Container {
	/**
	 * Gets the owning component for this nested container.
	 * @return The  that owns this nested container.
	 */
	var Owner(default, never):cs.system.componentmodel.IComponent;
	/**
	 * Gets the name of the owning component.
	 * @return The name of the owning component.
	 */
	var OwnerName(default, never):String;
	function new(owner:cs.system.componentmodel.IComponent):Void;
}

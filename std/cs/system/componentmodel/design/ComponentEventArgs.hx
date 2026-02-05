package cs.system.componentmodel.design;

/** Provides data for the , , , and  events. */
@:native("System.ComponentModel.Design.ComponentEventArgs")
extern class ComponentEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the component associated with the event.
	 * @return The component associated with the event.
	 */
	var Component(default, never):cs.system.componentmodel.IComponent;
	function new(component:cs.system.componentmodel.IComponent):Void;
}

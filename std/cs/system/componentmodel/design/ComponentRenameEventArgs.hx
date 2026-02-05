package cs.system.componentmodel.design;

/** Provides data for the  event. */
@:native("System.ComponentModel.Design.ComponentRenameEventArgs")
extern class ComponentRenameEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the component that is being renamed.
	 * @return The component that is being renamed.
	 */
	var Component(default, never):Dynamic;
	/**
	 * Gets the name of the component after the rename event.
	 * @return The name of the component after the rename event.
	 */
	var NewName(default, never):String;
	/**
	 * Gets the name of the component before the rename event.
	 * @return The previous name of the component.
	 */
	var OldName(default, never):String;
	function new(component:Dynamic, oldName:String, newName:String):Void;
}

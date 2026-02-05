package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.AddingNewEventArgs")
extern class AddingNewEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets the object to be added to the binding list.
	 * @return The  to be added as a new item to the associated collection.
	 */
	var NewObject(default, default):Dynamic;
	@:overload(function():Void {})
	function new(newObject:Dynamic):Void;
}

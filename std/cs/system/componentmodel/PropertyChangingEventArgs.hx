package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.PropertyChangingEventArgs")
extern class PropertyChangingEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the name of the property whose value is changing.
	 * @return The name of the property whose value is changing.
	 */
	var PropertyName(default, never):String;
	function new(propertyName:String):Void;
}

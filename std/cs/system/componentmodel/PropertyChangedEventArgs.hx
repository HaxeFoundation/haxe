package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.PropertyChangedEventArgs")
extern class PropertyChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the name of the property that changed.
	 * @return The name of the property that changed.
	 */
	var PropertyName(default, never):String;
	function new(propertyName:String):Void;
}

package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.DataErrorsChangedEventArgs")
extern class DataErrorsChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the name of the property that has an error.
	 * @return The name of the property that has an error.  or  if the error is
	 * object-level.
	 */
	var PropertyName(default, never):String;
	function new(propertyName:String):Void;
}

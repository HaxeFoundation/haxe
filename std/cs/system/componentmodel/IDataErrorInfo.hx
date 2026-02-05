package cs.system.componentmodel;

/** Provides the functionality to offer custom error information that a user interface can bind to. */
@:native("System.ComponentModel.IDataErrorInfo")
extern interface IDataErrorInfo {
	/**
	 * Gets an error message indicating what is wrong with this object.
	 * @return An error message indicating what is wrong with this object. The default
	 * is an empty string ("").
	 */
	var Error(default, never):String;
	@:native("get_Item")
	function get_Item(index0:String):String;
}

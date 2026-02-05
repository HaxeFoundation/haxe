package cs.system.componentmodel.design.serialization;

/** Provides data for the  event. */
@:native("System.ComponentModel.Design.Serialization.ResolveNameEventArgs")
extern class ResolveNameEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the name of the object to resolve.
	 * @return The name of the object to resolve.
	 */
	var Name(default, never):String;
	/**
	 * Gets or sets the object that matches the name.
	 * @return The object that the name is associated with.
	 */
	var Value(default, default):Dynamic;
	function new(name:String):Void;
}

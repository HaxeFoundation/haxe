package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.RefreshEventArgs")
extern class RefreshEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the component that changed its properties, events, or extenders.
	 * @return The component that changed its properties, events, or extenders, or  if
	 * all components of the same type have changed.
	 */
	var ComponentChanged(default, never):Dynamic;
	/**
	 * Gets the  that changed its properties or events.
	 * @return The  that changed its properties or events.
	 */
	var TypeChanged(default, never):cs.system.Type;
	@:overload(function(componentChanged:Dynamic):Void {})
	function new(typeChanged:cs.system.Type):Void;
}

package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.CollectionChangeEventArgs")
extern class CollectionChangeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets an action that specifies how the collection changed.
	 * @return One of the  values.
	 */
	var Action(default, never):cs.system.componentmodel.CollectionChangeAction;
	/**
	 * Gets the instance of the collection with the change.
	 * @return An  that represents the instance of the collection with the change, or 
	 * if you refresh the collection.
	 */
	var Element(default, never):Dynamic;
	function new(action:cs.system.componentmodel.CollectionChangeAction, element:Dynamic):Void;
}

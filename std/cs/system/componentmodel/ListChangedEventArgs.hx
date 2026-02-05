package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.ListChangedEventArgs")
extern class ListChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the type of change.
	 * @return A  value indicating the type of change.
	 */
	var ListChangedType(default, never):cs.system.componentmodel.ListChangedType;
	/**
	 * Gets the index of the item affected by the change.
	 * @return The index of the affected by the change.
	 */
	var NewIndex(default, never):Int;
	/**
	 * Gets the old index of an item that has been moved.
	 * @return The old index of the moved item.
	 */
	var OldIndex(default, never):Int;
	/**
	 * Gets the  that was added, changed, or deleted.
	 * @return The  affected by the change.
	 */
	var PropertyDescriptor(default, never):cs.system.componentmodel.PropertyDescriptor;
	@:overload(function(listChangedType:cs.system.componentmodel.ListChangedType, propDesc:cs.system.componentmodel.PropertyDescriptor):Void {})
	@:overload(function(listChangedType:cs.system.componentmodel.ListChangedType, newIndex:Int):Void {})
	@:overload(function(listChangedType:cs.system.componentmodel.ListChangedType, newIndex:Int, propDesc:cs.system.componentmodel.PropertyDescriptor):Void {})
	function new(listChangedType:cs.system.componentmodel.ListChangedType, newIndex:Int, oldIndex:Int):Void;
}

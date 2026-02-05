package cs.system.collections.specialized;

/** Provides data for the  event. */
@:native("System.Collections.Specialized.NotifyCollectionChangedEventArgs")
extern class NotifyCollectionChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the action that caused the event.
	 * @return A  value that describes the action that caused the event.
	 */
	var Action(default, never):cs.system.collections.specialized.NotifyCollectionChangedAction;
	/**
	 * Gets the list of new items involved in the change.
	 * @return The list of new items involved in the change.
	 */
	var NewItems(default, never):cs.system.collections.IList;
	/**
	 * Gets the index at which the change occurred.
	 * @return The zero-based index at which the change occurred.
	 */
	var NewStartingIndex(default, never):Int;
	/**
	 * Gets the list of items affected by a , Remove, or Move action.
	 * @return The list of items affected by a , Remove, or Move action.
	 */
	var OldItems(default, never):cs.system.collections.IList;
	/**
	 * Gets the index at which a , Remove, or Replace action occurred.
	 * @return The zero-based index at which a , Remove, or Replace action occurred.
	 */
	var OldStartingIndex(default, never):Int;
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItems:cs.system.collections.IList):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItem:Dynamic):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, newItems:cs.system.collections.IList, oldItems:cs.system.collections.IList):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItems:cs.system.collections.IList, startingIndex:Int):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItem:Dynamic, index:Int):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, newItem:Dynamic, oldItem:Dynamic):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, newItems:cs.system.collections.IList, oldItems:cs.system.collections.IList, startingIndex:Int):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItems:cs.system.collections.IList, index:Int, oldIndex:Int):Void {})
	@:overload(function(action:cs.system.collections.specialized.NotifyCollectionChangedAction, changedItem:Dynamic, index:Int, oldIndex:Int):Void {})
	function new(action:cs.system.collections.specialized.NotifyCollectionChangedAction, newItem:Dynamic, oldItem:Dynamic, index:Int):Void;
}

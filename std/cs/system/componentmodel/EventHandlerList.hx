package cs.system.componentmodel;

/** Provides a simple list of delegates. This class cannot be inherited. */
@:native("System.ComponentModel.EventHandlerList")
extern class EventHandlerList {
	@:native("get_Item")
	function get_Item(index0:Dynamic):cs.system.Delegate;
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:cs.system.Delegate):Void;
	function new():Void;
	/**
	 * Adds a delegate to the list.
	 * @param key The object that owns the event.
	 * @param value The delegate to add to the list.
	 */
	function AddHandler(key:Dynamic, value:cs.system.Delegate):Void;
	/**
	 * Adds a list of delegates to the current list.
	 * @param listToAddFrom The list to add.
	 */
	function AddHandlers(listToAddFrom:cs.system.componentmodel.EventHandlerList):Void;
	/** Disposes the delegate list. */
	function Dispose():Void;
	/**
	 * Removes a delegate from the list.
	 * @param key The object that owns the event.
	 * @param value The delegate to remove from the list.
	 */
	function RemoveHandler(key:Dynamic, value:cs.system.Delegate):Void;
}

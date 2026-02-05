package cs.system.componentmodel.design;

/** Represents a Windows menu or toolbar command item. */
@:native("System.ComponentModel.Design.MenuCommand")
extern class MenuCommand {
	/**
	 * Gets or sets a value indicating whether this menu item is checked.
	 * @return if the item is checked; otherwise, .
	 */
	var Checked(default, default):Bool;
	/**
	 * Gets the  associated with this menu command.
	 * @return The  associated with the menu command.
	 */
	var CommandID(default, never):cs.system.componentmodel.design.CommandID;
	/**
	 * Gets a value indicating whether this menu item is available.
	 * @return if the item is enabled; otherwise, .
	 */
	var Enabled(default, default):Bool;
	/**
	 * Gets the OLE command status code for this menu item.
	 * @return An integer containing a mixture of status flags that reflect the state
	 * of this menu item.
	 */
	var OleStatus(default, never):Int;
	/**
	 * Gets the public properties associated with the .
	 * @return An  containing the public properties of the .
	 */
	var Properties(default, never):cs.system.collections.IDictionary;
	/**
	 * Gets or sets a value indicating whether this menu item is supported.
	 * @return if the item is supported, which is the default; otherwise, .
	 */
	var Supported(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether this menu item is visible.
	 * @return if the item is visible; otherwise, .
	 */
	var Visible(default, default):Bool;
	function new(handler:cs.system.EventHandler, command:cs.system.componentmodel.design.CommandID):Void;
	@:overload(function():Void {})
	/** Invokes the command. */
	function Invoke(arg:Dynamic):Void;
	/**
	 * Returns a string representation of this menu command.
	 * @return A string containing the value of the  property appended with the names
	 * of any flags that are set, separated by pipe bars (|). These flag properties
	 * include , , , and .
	 */
	function ToString():String;
}

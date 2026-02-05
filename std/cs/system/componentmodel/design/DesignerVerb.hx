package cs.system.componentmodel.design;

/** Represents a verb that can be invoked from a designer. */
@:native("System.ComponentModel.Design.DesignerVerb")
extern class DesignerVerb extends cs.system.componentmodel.design.MenuCommand {
	/**
	 * Gets or sets the description of the menu item for the verb.
	 * @return A string describing the menu item.
	 */
	var Description(default, default):String;
	/**
	 * Gets the text description for the verb command on the menu.
	 * @return A description for the verb command.
	 */
	var Text(default, never):String;
	@:overload(function(text:String, handler:cs.system.EventHandler):Void {})
	function new(text:String, handler:cs.system.EventHandler, startCommandID:cs.system.componentmodel.design.CommandID):Void;
	/**
	 * Overrides .
	 * @return The verb's text, or an empty string ("") if the text field is empty.
	 */
	function ToString():String;
}

package cs.system.componentmodel.design;

/** Provides methods to manage the global designer verbs and menu commands available in design mode, and to show some types of shortcut menus. */
@:native("System.ComponentModel.Design.IMenuCommandService")
extern interface IMenuCommandService {
	/**
	 * Gets a collection of the designer verbs that are currently available.
	 * @return A  that contains the designer verbs that are currently available.
	 */
	var Verbs(default, never):cs.system.componentmodel.design.DesignerVerbCollection;
	/**
	 * Adds the specified standard menu command to the menu.
	 * @param command The  to add.
	 */
	function AddCommand(command:cs.system.componentmodel.design.MenuCommand):Void;
	/**
	 * Adds the specified designer verb to the set of global designer verbs.
	 * @param verb The  to add.
	 */
	function AddVerb(verb:cs.system.componentmodel.design.DesignerVerb):Void;
	/**
	 * Searches for the specified command ID and returns the menu command associated
	 * with it.
	 * @param commandID The  to search for.
	 * @return The  associated with the command ID, or  if no command is found.
	 */
	function FindCommand(commandID:cs.system.componentmodel.design.CommandID):cs.system.componentmodel.design.MenuCommand;
	/**
	 * Invokes a menu or designer verb command matching the specified command ID.
	 * @param commandID The  of the command to search for and execute.
	 * @return if the command was found and invoked successfully; otherwise, .
	 */
	function GlobalInvoke(commandID:cs.system.componentmodel.design.CommandID):Bool;
	/**
	 * Removes the specified standard menu command from the menu.
	 * @param command The  to remove.
	 */
	function RemoveCommand(command:cs.system.componentmodel.design.MenuCommand):Void;
	/**
	 * Removes the specified designer verb from the collection of global designer
	 * verbs.
	 * @param verb The  to remove.
	 */
	function RemoveVerb(verb:cs.system.componentmodel.design.DesignerVerb):Void;
	/**
	 * Shows the specified shortcut menu at the specified location.
	 * @param menuID The  for the shortcut menu to show.
	 * @param x The x-coordinate at which to display the menu, in screen coordinates.
	 * @param y The y-coordinate at which to display the menu, in screen coordinates.
	 */
	function ShowContextMenu(menuID:cs.system.componentmodel.design.CommandID, x:Int, y:Int):Void;
}

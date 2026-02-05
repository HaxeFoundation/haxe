package cs.system.componentmodel.design;

/** Represents a unique command identifier that consists of a numeric command ID and a GUID menu group identifier. */
@:native("System.ComponentModel.Design.CommandID")
extern class CommandID {
	/**
	 * Gets the GUID of the menu group that the menu command identified by this 
	 * belongs to.
	 * @return The GUID of the command group for this command.
	 */
	var Guid(default, never):cs.system.Guid;
	/**
	 * Gets the numeric command ID.
	 * @return The command ID number.
	 */
	var ID(default, never):Int;
	function new(menuGroup:cs.system.Guid, commandID:Int):Void;
	/**
	 * Determines whether two  instances are equal.
	 * @param obj The object to compare.
	 * @return if the specified object is equivalent to this one; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for a particular type.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a  that represents the current object.
	 * @return A string that contains the command ID information, both the GUID and
	 * integer identifier.
	 */
	function ToString():String;
}

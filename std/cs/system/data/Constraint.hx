package cs.system.data;

/** Represents a constraint that can be enforced on one or more  objects. */
@:native("System.Data.Constraint")
extern class Constraint {
	/**
	 * Gets the  to which this constraint belongs.
	 * @return The  to which the constraint belongs.
	 */
	var _DataSet(default, never):cs.system.data.DataSet;
	/**
	 * The name of a constraint in the .
	 * @return The name of the .
	 */
	var ConstraintName(default, default):String;
	/**
	 * Gets the collection of user-defined constraint properties.
	 * @return A  of custom information.
	 */
	var ExtendedProperties(default, never):cs.system.data.PropertyCollection;
	/**
	 * Gets the  to which the constraint applies.
	 * @return A  to which the constraint applies.
	 */
	var Table(default, never):cs.system.data.DataTable;
	/**
	 * Gets the , if there is one, as a string.
	 * @return The string value of the .
	 */
	function ToString():String;
}

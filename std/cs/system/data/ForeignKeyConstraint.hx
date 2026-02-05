package cs.system.data;

/** Represents an action restriction enforced on a set of columns in a primary key/foreign key relationship when a value or row is either deleted or updated. */
@:native("System.Data.ForeignKeyConstraint")
extern class ForeignKeyConstraint extends cs.system.data.Constraint {
	/**
	 * Indicates the action that should take place across this constraint when  is
	 * invoked.
	 * @return One of the  values. Possible values include , and . The default is .
	 */
	var AcceptRejectRule(default, default):cs.system.data.AcceptRejectRule;
	/**
	 * Gets the child columns of this constraint.
	 * @return An array of  objects that are the child columns of the constraint.
	 */
	var Columns(default, never):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets or sets the action that occurs across this constraint when a row is
	 * deleted.
	 * @return One of the  values. The default is .
	 */
	var DeleteRule(default, default):cs.system.data.Rule;
	/**
	 * The parent columns of this constraint.
	 * @return An array of  objects that are the parent columns of the constraint.
	 */
	var RelatedColumns(default, never):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets the parent table of this constraint.
	 * @return The parent  of this constraint.
	 */
	var RelatedTable(default, never):cs.system.data.DataTable;
	/**
	 * Gets or sets the action that occurs across this constraint on when a row is
	 * updated.
	 * @return One of the  values. The default is .
	 */
	var UpdateRule(default, default):cs.system.data.Rule;
	@:overload(function(parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn):Void {})
	@:overload(function(parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>):Void {})
	@:overload(function(constraintName:String, parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn):Void {})
	@:overload(function(constraintName:String, parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>):Void {})
	@:overload(function(constraintName:String, parentTableName:String, parentColumnNames:cs.NativeArray<String>, childColumnNames:cs.NativeArray<String>, acceptRejectRule:cs.system.data.AcceptRejectRule, deleteRule:cs.system.data.Rule, updateRule:cs.system.data.Rule):Void {})
	function new(constraintName:String, parentTableName:String, parentTableNamespace:String, parentColumnNames:cs.NativeArray<String>, childColumnNames:cs.NativeArray<String>, acceptRejectRule:cs.system.data.AcceptRejectRule, deleteRule:cs.system.data.Rule, updateRule:cs.system.data.Rule):Void;
	/**
	 * Gets a value indicating whether the current  is identical to the specified
	 * object.
	 * @param key The object to which this  is compared. Two  are equal if they
	 * constrain the same columns.
	 * @return , if the objects are identical; otherwise, .
	 */
	function Equals(key:Dynamic):Bool;
	/**
	 * Gets the hash code of this instance of the  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}

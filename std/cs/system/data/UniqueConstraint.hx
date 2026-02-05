package cs.system.data;

/** Represents a restriction on a set of columns in which all values must be unique. */
@:native("System.Data.UniqueConstraint")
extern class UniqueConstraint extends cs.system.data.Constraint {
	/**
	 * Gets the array of columns that this constraint affects.
	 * @return An array of  objects.
	 */
	var Columns(default, never):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets a value indicating whether or not the constraint is on a primary key.
	 * @return , if the constraint is on a primary key; otherwise, .
	 */
	var IsPrimaryKey(default, never):Bool;
	@:overload(function(column:cs.system.data.DataColumn):Void {})
	@:overload(function(columns:cs.NativeArray<cs.system.data.DataColumn>):Void {})
	@:overload(function(column:cs.system.data.DataColumn, isPrimaryKey:Bool):Void {})
	@:overload(function(columns:cs.NativeArray<cs.system.data.DataColumn>, isPrimaryKey:Bool):Void {})
	@:overload(function(name:String, column:cs.system.data.DataColumn):Void {})
	@:overload(function(name:String, columns:cs.NativeArray<cs.system.data.DataColumn>):Void {})
	@:overload(function(name:String, column:cs.system.data.DataColumn, isPrimaryKey:Bool):Void {})
	@:overload(function(name:String, columns:cs.NativeArray<cs.system.data.DataColumn>, isPrimaryKey:Bool):Void {})
	function new(name:String, columnNames:cs.NativeArray<String>, isPrimaryKey:Bool):Void;
	/**
	 * Compares this constraint to a second to determine if both are identical.
	 * @param key2 The object to which this  is compared.
	 * @return , if the constraints are equal; otherwise, .
	 */
	function Equals(key2:Dynamic):Bool;
	/**
	 * Gets the hash code of this instance of the  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}

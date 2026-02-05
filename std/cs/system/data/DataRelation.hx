package cs.system.data;

/** Represents a parent/child relationship between two  objects. */
@:native("System.Data.DataRelation")
extern class DataRelation {
	/**
	 * Gets the child  objects of this relation.
	 * @return An array of  objects.
	 */
	var ChildColumns(default, never):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets the  for the relation.
	 * @return A .
	 */
	var ChildKeyConstraint(default, never):cs.system.data.ForeignKeyConstraint;
	/**
	 * Gets the child table of this relation.
	 * @return A  that is the child table of the relation.
	 */
	var ChildTable(default, never):cs.system.data.DataTable;
	/**
	 * Gets the  to which the  belongs.
	 * @return A  to which the  belongs.
	 */
	var DataSet(default, never):cs.system.data.DataSet;
	/**
	 * Gets the collection that stores customized properties.
	 * @return A  that contains customized properties.
	 */
	var ExtendedProperties(default, never):cs.system.data.PropertyCollection;
	/**
	 * Gets or sets a value that indicates whether  objects are nested.
	 * @return , if  objects are nested; otherwise, .
	 */
	var Nested(default, default):Bool;
	/**
	 * Gets an array of  objects that are the parent columns of this .
	 * @return An array of  objects that are the parent columns of this .
	 */
	var ParentColumns(default, never):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets the  that guarantees that values in the parent column of a  are unique.
	 * @return A  that makes sure that values in a parent column are unique.
	 */
	var ParentKeyConstraint(default, never):cs.system.data.UniqueConstraint;
	/**
	 * Gets the parent  of this .
	 * @return A  that is the parent table of this relation.
	 */
	var ParentTable(default, never):cs.system.data.DataTable;
	/**
	 * Gets or sets the name used to retrieve a  from the .
	 * @return The name of the a .
	 */
	var RelationName(default, default):String;
	@:overload(function(relationName:String, parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn):Void {})
	@:overload(function(relationName:String, parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>):Void {})
	@:overload(function(relationName:String, parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn, createConstraints:Bool):Void {})
	@:overload(function(relationName:String, parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>, createConstraints:Bool):Void {})
	@:overload(function(relationName:String, parentTableName:String, childTableName:String, parentColumnNames:cs.NativeArray<String>, childColumnNames:cs.NativeArray<String>, nested:Bool):Void {})
	function new(relationName:String, parentTableName:String, parentTableNamespace:String, childTableName:String, childTableNamespace:String, parentColumnNames:cs.NativeArray<String>, childColumnNames:cs.NativeArray<String>, nested:Bool):Void;
	/**
	 * Gets the , if one exists.
	 * @return The value of the  property.
	 */
	function ToString():String;
}

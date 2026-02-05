package cs.system.data;

/** Represents the collection of  objects for this . */
@:native("System.Data.DataRelationCollection")
extern class DataRelationCollection extends cs.system.data.InternalDataCollectionBase {
	@:overload(function(index0:Int):cs.system.data.DataRelation {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.DataRelation;
	@:overload(function(relation:cs.system.data.DataRelation):Void {})
	@:overload(function(parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn):cs.system.data.DataRelation {})
	@:overload(function(parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>):cs.system.data.DataRelation {})
	@:overload(function(name:String, parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn):cs.system.data.DataRelation {})
	@:overload(function(name:String, parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>):cs.system.data.DataRelation {})
	@:overload(function(name:String, parentColumn:cs.system.data.DataColumn, childColumn:cs.system.data.DataColumn, createConstraints:Bool):cs.system.data.DataRelation {})
	/**
	 * Creates a  with a specified parent and child column, and adds it to the
	 * collection.
	 * @param parentColumn The parent column of the relation.
	 * @param childColumn The child column of the relation.
	 * @return The created relation.
	 */
	function Add(name:String, parentColumns:cs.NativeArray<cs.system.data.DataColumn>, childColumns:cs.NativeArray<cs.system.data.DataColumn>, createConstraints:Bool):cs.system.data.DataRelation;
	/**
	 * Copies the elements of the specified  array to the end of the collection.
	 * @param relations The array of  objects to add to the collection.
	 */
	function AddRange(relations:cs.NativeArray<cs.system.data.DataRelation>):Void;
	/**
	 * Verifies whether the specified  can be removed from the collection.
	 * @param relation The relation to perform the check against.
	 * @return if the  can be removed; otherwise, .
	 */
	function CanRemove(relation:cs.system.data.DataRelation):Bool;
	/** Clears the collection of any relations. */
	function Clear():Void;
	/**
	 * Verifies whether a  with the specific name (case insensitive) exists in the
	 * collection.
	 * @param name The name of the relation to find.
	 * @return , if a relation with the specified name exists; otherwise .
	 */
	function Contains(name:String):Bool;
	/**
	 * Copies the collection of  objects starting at the specified index.
	 * @param array The array of  objects to copy the collection to.
	 * @param index The index to start from.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.DataRelation>, index:Int):Void;
	@:overload(function(relation:cs.system.data.DataRelation):Int {})
	/**
	 * Gets the index of the specified  object.
	 * @param relation The relation to search for.
	 * @return The 0-based index of the relation, or -1 if the relation is not found in
	 * the collection.
	 */
	function IndexOf(relationName:String):Int;
	@:overload(function(relation:cs.system.data.DataRelation):Void {})
	/**
	 * Removes the specified relation from the collection.
	 * @param relation The relation to remove.
	 */
	function Remove(name:String):Void;
	/**
	 * Removes the relation at the specified index from the collection.
	 * @param index The index of the relation to remove.
	 */
	function RemoveAt(index:Int):Void;
}

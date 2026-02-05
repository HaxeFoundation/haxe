package cs.system.data;

/** Represents a collection of constraints for a . */
@:native("System.Data.ConstraintCollection")
extern class ConstraintCollection extends cs.system.data.InternalDataCollectionBase {
	@:overload(function(index0:Int):cs.system.data.Constraint {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.Constraint;
	@:overload(function(constraint:cs.system.data.Constraint):Void {})
	@:overload(function(name:String, column:cs.system.data.DataColumn, primaryKey:Bool):cs.system.data.Constraint {})
	@:overload(function(name:String, primaryKeyColumn:cs.system.data.DataColumn, foreignKeyColumn:cs.system.data.DataColumn):cs.system.data.Constraint {})
	@:overload(function(name:String, columns:cs.NativeArray<cs.system.data.DataColumn>, primaryKey:Bool):cs.system.data.Constraint {})
	/**
	 * Adds the specified  object to the collection.
	 * @param constraint The  to add.
	 */
	function Add(name:String, primaryKeyColumns:cs.NativeArray<cs.system.data.DataColumn>, foreignKeyColumns:cs.NativeArray<cs.system.data.DataColumn>):cs.system.data.Constraint;
	/**
	 * Copies the elements of the specified  array to the end of the collection.
	 * @param constraints An array of  objects to add to the collection.
	 */
	function AddRange(constraints:cs.NativeArray<cs.system.data.Constraint>):Void;
	/**
	 * Indicates whether a  can be removed.
	 * @param constraint The  to be tested for removal from the collection.
	 * @return if the  can be removed from collection; otherwise, .
	 */
	function CanRemove(constraint:cs.system.data.Constraint):Bool;
	/** Clears the collection of any  objects. */
	function Clear():Void;
	/**
	 * Indicates whether the  object specified by name exists in the collection.
	 * @param name The  of the constraint.
	 * @return if the collection contains the specified constraint; otherwise, .
	 */
	function Contains(name:String):Bool;
	/**
	 * Copies the collection objects to a one-dimensional  instance starting at the
	 * specified index.
	 * @param array The one-dimensional  that is the destination of the values copied
	 * from the collection.
	 * @param index The index of the array at which to start inserting.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.Constraint>, index:Int):Void;
	@:overload(function(constraint:cs.system.data.Constraint):Int {})
	/**
	 * Gets the index of the specified .
	 * @param constraint The  to search for.
	 * @return The zero-based index of the  if it is in the collection; otherwise, -1.
	 */
	function IndexOf(constraintName:String):Int;
	@:overload(function(constraint:cs.system.data.Constraint):Void {})
	/**
	 * Removes the specified  from the collection.
	 * @param constraint The  to remove.
	 */
	function Remove(name:String):Void;
	/**
	 * Removes the  object at the specified index from the collection.
	 * @param index The index of the  to remove.
	 */
	function RemoveAt(index:Int):Void;
}

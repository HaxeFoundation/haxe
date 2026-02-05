package cs.system.data.common;

/** The base class for a collection of parameters relevant to a . */
@:native("System.Data.Common.DbParameterCollection")
extern class DbParameterCollection extends cs.system.MarshalByRefObject {
	/**
	 * Specifies the number of items in the collection.
	 * @return The number of items in the collection.
	 */
	var Count(default, never):Int;
	/**
	 * Specifies whether the collection is a fixed size.
	 * @return if the collection is a fixed size; otherwise .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Specifies whether the collection is read-only.
	 * @return if the collection is read-only; otherwise .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Specifies whether the collection is synchronized.
	 * @return if the collection is synchronized; otherwise .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Specifies the  to be used to synchronize access to the collection.
	 * @return A  to be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:Int):cs.system.data.common.DbParameter {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.common.DbParameter;
	@:overload(function(index0:Int, value:cs.system.data.common.DbParameter):Void {})
	@:native("set_Item")
	function set_Item(index0:String, value:cs.system.data.common.DbParameter):Void;
	/**
	 * Adds the specified  object to the .
	 * @param value The  of the  to add to the collection.
	 * @return The index of the  object in the collection.
	 */
	function Add(value:Dynamic):Int;
	/**
	 * Adds an array of items with the specified values to the .
	 * @param values An array of values of type  to add to the collection.
	 */
	function AddRange(values:cs.system.Array):Void;
	/** Removes all  values from the . */
	function Clear():Void;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Indicates whether a  with the specified  is contained in the collection.
	 * @param value The  of the  to look for in the collection.
	 * @return if the  is in the collection; otherwise .
	 */
	function Contains(value:String):Bool;
	/**
	 * Copies an array of items to the collection starting at the specified index.
	 * @param array The array of items to copy to the collection.
	 * @param index The index in the collection to copy the items.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Exposes the  method, which supports a simple iteration over a collection by a
	 * .NET Framework data provider.
	 * @return An  that can be used to iterate through the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Returns the index of the specified  object.
	 * @param value The  object in the collection.
	 * @return The index of the specified  object.
	 */
	function IndexOf(parameterName:String):Int;
	/**
	 * Inserts the specified index of the  object with the specified name into the
	 * collection at the specified index.
	 * @param index The index at which to insert the  object.
	 * @param value The  object to insert into the collection.
	 */
	function Insert(index:Int, value:Dynamic):Void;
	/**
	 * Removes the specified  object from the collection.
	 * @param value The  object to remove.
	 */
	function Remove(value:Dynamic):Void;
	@:overload(function(index:Int):Void {})
	/**
	 * Removes the  object at the specified from the collection.
	 * @param index The index where the  object is located.
	 */
	function RemoveAt(parameterName:String):Void;
}

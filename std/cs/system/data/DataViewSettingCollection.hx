package cs.system.data;

/** Contains a read-only collection of  objects for each  in a . */
@:native("System.Data.DataViewSettingCollection")
extern class DataViewSettingCollection {
	/**
	 * Gets the number of  objects in the .
	 * @return The number of  objects in the collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether the  is read-only.
	 * @return Always returns  to indicate the collection is read-only.
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value that indicates whether access to the  is synchronized
	 * (thread-safe).
	 * @return This property is always , unless overridden by a derived class.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:cs.system.data.DataTable):cs.system.data.DataViewSetting {})
	@:overload(function(index0:Int):cs.system.data.DataViewSetting {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.DataViewSetting;
	@:overload(function(index0:cs.system.data.DataTable, value:cs.system.data.DataViewSetting):Void {})
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.data.DataViewSetting):Void;
	@:overload(function(ar:cs.system.Array, index:Int):Void {})
	/**
	 * Copies the collection objects to a one-dimensional  instance starting at the
	 * specified index.
	 * @param ar The one-dimensional  that is the destination of the values copied from
	 * the collection.
	 * @param index The index of the array at which to start inserting.
	 */
	function CopyTo(ar:cs.NativeArray<cs.system.data.DataViewSetting>, index:Int):Void;
	/**
	 * Gets an  for the collection.
	 * @return An  object.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}

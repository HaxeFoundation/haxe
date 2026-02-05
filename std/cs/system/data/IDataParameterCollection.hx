package cs.system.data;

/** Collects all parameters relevant to a Command object and their mappings to  columns, and is implemented by .NET Framework data providers that access data sources. */
@:native("System.Data.IDataParameterCollection")
extern interface IDataParameterCollection extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IList {
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	/**
	 * Gets a value indicating whether a parameter in the collection has the specified
	 * name.
	 * @param parameterName The name of the parameter.
	 * @return if the collection contains the parameter; otherwise, .
	 */
	function Contains(parameterName:String):Bool;
	/**
	 * Gets the location of the  within the collection.
	 * @param parameterName The name of the parameter.
	 * @return The zero-based location of the  within the collection.
	 */
	function IndexOf(parameterName:String):Int;
	/**
	 * Removes the  from the collection.
	 * @param parameterName The name of the parameter.
	 */
	function RemoveAt(parameterName:String):Void;
}

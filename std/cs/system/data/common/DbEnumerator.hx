package cs.system.data.common;

/** Exposes the  method, which supports a simple iteration over a collection by a .NET Framework data provider. */
@:native("System.Data.Common.DbEnumerator")
extern class DbEnumerator {
	/**
	 * Gets the current element in the collection.
	 * @return The current element in the collection.
	 */
	var Current(default, never):Dynamic;
	@:overload(function(reader:cs.system.data.common.DbDataReader):Void {})
	@:overload(function(reader:cs.system.data.IDataReader):Void {})
	@:overload(function(reader:cs.system.data.common.DbDataReader, closeReader:Bool):Void {})
	function new(reader:cs.system.data.IDataReader, closeReader:Bool):Void;
	/**
	 * Advances the enumerator to the next element of the collection.
	 * @return if the enumerator was successfully advanced to the next element;  if the
	 * enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first element in the collection. */
	function Reset():Void;
}

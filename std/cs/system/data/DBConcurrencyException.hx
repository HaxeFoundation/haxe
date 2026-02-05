package cs.system.data;

/** The exception that is thrown by the  during an insert, update, or delete operation if the number of rows affected equals zero. */
@:native("System.Data.DBConcurrencyException")
extern class DBConcurrencyException extends cs.system.SystemException {
	/**
	 * Gets or sets the value of the  that generated the .
	 * @return The value of the .
	 */
	var Row(default, default):cs.system.data.DataRow;
	/**
	 * Gets the number of rows whose update failed, generating this exception.
	 * @return An integer containing a count of the number of rows whose update failed.
	 */
	var RowCount(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(message:String, inner:cs.system.Exception, dataRows:cs.NativeArray<cs.system.data.DataRow>):Void;
	@:overload(function(array:cs.NativeArray<cs.system.data.DataRow>):Void {})
	/**
	 * Copies the  objects whose update failure generated this exception, to the
	 * specified array of  objects.
	 * @param array The one-dimensional array of  objects to copy the  objects into.
	 */
	function CopyToRows(array:cs.NativeArray<cs.system.data.DataRow>, arrayIndex:Int):Void;
	/**
	 * Populates the specified serialization information object with the data needed to
	 * serialize the .
	 * @param si A  that holds the serialized data associated with the .
	 * @param context A  that contains the source and destination of the serialized
	 * stream associated with the .
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}

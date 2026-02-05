package cs.system.net;

/** Contains protocol headers associated with a request or response. */
@:native("System.Net.WebHeaderCollection")
extern class WebHeaderCollection extends cs.system.collections.specialized.NameValueCollection {
	function new():Void;
	@:overload(function(headerName:String):Bool {})
	/**
	 * Tests whether the specified HTTP header can be set for the request.
	 * @param headerName The header to test.
	 * @return if the header is restricted; otherwise .
	 */
	static function IsRestricted(headerName:String, response:Bool):Bool;
	@:overload(function(header:String):Void {})
	@:overload(function(header:cs.system.net.HttpRequestHeader, value:String):Void {})
	@:overload(function(header:cs.system.net.HttpResponseHeader, value:String):Void {})
	/**
	 * Inserts the specified header with the specified value into the collection.
	 * @param header The header to add to the collection.
	 * @param value The content of the header.
	 */
	function Add(name:String, value:String):Void;
	/** Removes all headers from the collection. */
	function Clear():Void;
	@:overload(function(index:Int):String {})
	/**
	 * Gets the value of a particular header in the collection, specified by an index
	 * into the collection.
	 * @param index The zero-based index of the key to get from the collection.
	 * @return A  containing the value of the specified header.
	 */
	function Get(name:String):String;
	/**
	 * Returns an enumerator that can iterate through the  instance.
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets the header name at the specified position in the collection.
	 * @param index The zero-based index of the key to get from the collection.
	 * @return A  holding the header name.
	 */
	function GetKey(index:Int):String;
	/**
	 * Populates a  with the data needed to serialize the target object.
	 * @param serializationInfo The  to populate with data.
	 * @param streamingContext A  that specifies the destination for this
	 * serialization.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
	@:overload(function(index:Int):cs.NativeArray<String> {})
	/**
	 * Gets an array of header values stored in the  position of the header collection.
	 * @param index The header index to return.
	 * @return An array of header strings.
	 */
	function GetValues(header:String):cs.NativeArray<String>;
	/**
	 * Implements the  interface and raises the deserialization event when the
	 * deserialization is complete.
	 * @param sender The source of the deserialization event.
	 */
	function OnDeserialization(sender:Dynamic):Void;
	@:overload(function(header:cs.system.net.HttpRequestHeader):Void {})
	@:overload(function(header:cs.system.net.HttpResponseHeader):Void {})
	/**
	 * Removes the specified header from the collection.
	 * @param header The  instance to remove from the collection.
	 */
	function Remove(name:String):Void;
	@:overload(function(header:cs.system.net.HttpRequestHeader, value:String):Void {})
	@:overload(function(header:cs.system.net.HttpResponseHeader, value:String):Void {})
	/**
	 * Sets the specified header to the specified value.
	 * @param header The  value to set.
	 * @param value The content of the header to set.
	 */
	function Set(name:String, value:String):Void;
	/**
	 * Converts the  to a byte array.
	 * @return A  array holding the header collection.
	 */
	function ToByteArray():cs.NativeArray<cs.UInt8>;
	/**
	 * This method is obsolete.
	 * @return The  representation of the collection.
	 */
	function ToString():String;
}

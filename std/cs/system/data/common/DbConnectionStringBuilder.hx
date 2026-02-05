package cs.system.data.common;

/** Provides a base class for strongly typed connection string builders. */
@:native("System.Data.Common.DbConnectionStringBuilder")
extern class DbConnectionStringBuilder {
	/**
	 * Gets or sets a value that indicates whether the  property is visible in Visual
	 * Studio designers.
	 * @return if the connection string is visible within designers;  otherwise. The
	 * default is .
	 */
	var BrowsableConnectionString(default, default):Bool;
	/**
	 * Gets or sets the connection string associated with the .
	 * @return The current connection string, created from the key/value pairs that are
	 * contained within the . The default value is an empty string.
	 */
	var ConnectionString(default, default):String;
	/**
	 * Gets the current number of keys that are contained within the  property.
	 * @return The number of keys that are contained within the connection string
	 * maintained by the  instance.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether the  has a fixed size.
	 * @return if the  has a fixed size; otherwise .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value that indicates whether the  is read-only.
	 * @return if the  is read-only; otherwise . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets an  that contains the keys in the .
	 * @return An  that contains the keys in the .
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an  that contains the values in the .
	 * @return An  that contains the values in the .
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	@:overload(function():Void {})
	function new(useOdbcRules:Bool):Void;
	@:overload(function(builder:cs.system.text.StringBuilder, keyword:String, value:String):Void {})
	/**
	 * Provides an efficient and safe way to append a key and value to an existing 
	 * object.
	 * @param builder The  to which to add the key/value pair.
	 * @param keyword The key to be added.
	 * @param value The value for the supplied key.
	 */
	static function AppendKeyValuePair(builder:cs.system.text.StringBuilder, keyword:String, value:String, useOdbcRules:Bool):Void;
	/**
	 * Adds an entry with the specified key and value into the .
	 * @param keyword The key to add to the .
	 * @param value The value for the specified key.
	 */
	function Add(keyword:String, value:Dynamic):Void;
	/** Clears the contents of the  instance. */
	function Clear():Void;
	/**
	 * Determines whether the  contains a specific key.
	 * @param keyword The key to locate in the .
	 * @return if the  contains an entry with the specified key; otherwise .
	 */
	function ContainsKey(keyword:String):Bool;
	/**
	 * Compares the connection information in this  object with the connection
	 * information in the supplied object.
	 * @param connectionStringBuilder The  to be compared with this  object.
	 * @return if the connection information in both of the  objects causes an
	 * equivalent connection string; otherwise .
	 */
	function EquivalentTo(connectionStringBuilder:cs.system.data.common.DbConnectionStringBuilder):Bool;
	/**
	 * Removes the entry with the specified key from the  instance.
	 * @param keyword The key of the key/value pair to be removed from the connection
	 * string in this .
	 * @return if the key existed within the connection string and was removed;  if the
	 * key did not exist.
	 */
	function Remove(keyword:String):Bool;
	/**
	 * Indicates whether the specified key exists in this  instance.
	 * @param keyword The key to locate in the .
	 * @return if the  contains an entry with the specified key; otherwise .
	 */
	function ShouldSerialize(keyword:String):Bool;
	/**
	 * Returns the connection string associated with this .
	 * @return The current  property.
	 */
	function ToString():String;
	/**
	 * Retrieves a value corresponding to the supplied key from this .
	 * @param keyword The key of the item to retrieve.
	 * @param value The value corresponding to the .
	 * @return if  was found within the connection string,  otherwise.
	 */
	function TryGetValue(keyword:String, value:cs.Ref<Dynamic>):Bool;
}

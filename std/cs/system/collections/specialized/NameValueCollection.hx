package cs.system.collections.specialized;

/** Represents a collection of associated  keys and  values that can be accessed either with the key or with the index. */
@:native("System.Collections.Specialized.NameValueCollection")
extern class NameValueCollection extends cs.system.collections.specialized.NameObjectCollectionBase {
	/**
	 * Gets all the keys in the .
	 * @return A  array that contains all the keys of the .
	 */
	var AllKeys(default, never):cs.NativeArray<String>;
	@:overload(function(index0:Int):String {})
	@:native("get_Item")
	function get_Item(index0:String):String;
	@:native("set_Item")
	function set_Item(index0:String, value:String):Void;
	@:overload(function():Void {})
	@:overload(function(equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(col:cs.system.collections.specialized.NameValueCollection):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(hashProvider:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(capacity:Int, equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(capacity:Int, col:cs.system.collections.specialized.NameValueCollection):Void {})
	function new(capacity:Int, hashProvider:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void;
	@:overload(function(c:cs.system.collections.specialized.NameValueCollection):Void {})
	/**
	 * Copies the entries in the specified  to the current .
	 * @param c The  to copy to the current .
	 */
	function Add(name:String, value:String):Void;
	/** Invalidates the cached arrays and removes all entries from the . */
	function Clear():Void;
	/**
	 * Copies the entire  to a compatible one-dimensional , starting at the specified
	 * index of the target array.
	 * @param dest The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(dest:cs.system.Array, index:Int):Void;
	@:overload(function(index:Int):String {})
	/**
	 * Gets the values at the specified index of the  combined into one comma-separated
	 * list.
	 * @param index The zero-based index of the entry that contains the values to get
	 * from the collection.
	 * @return A  that contains a comma-separated list of the values at the specified
	 * index of the , if found; otherwise, .
	 */
	function Get(name:String):String;
	/**
	 * Gets the key at the specified index of the .
	 * @param index The zero-based index of the key to get from the collection.
	 * @return A  that contains the key at the specified index of the , if found;
	 * otherwise, .
	 */
	function GetKey(index:Int):String;
	@:overload(function(index:Int):cs.NativeArray<String> {})
	/**
	 * Gets the values at the specified index of the .
	 * @param index The zero-based index of the entry that contains the values to get
	 * from the collection.
	 * @return A  array that contains the values at the specified index of the , if
	 * found; otherwise, .
	 */
	function GetValues(name:String):cs.NativeArray<String>;
	/**
	 * Gets a value indicating whether the  contains keys that are not .
	 * @return if the  contains keys that are not ; otherwise, .
	 */
	function HasKeys():Bool;
	/**
	 * Removes the entries with the specified key from the  instance.
	 * @param name The  key of the entry to remove. The key can be .
	 */
	function Remove(name:String):Void;
	/**
	 * Sets the value of an entry in the .
	 * @param name The  key of the entry to add the new value to. The key can be .
	 * @param value The  that represents the new value to add to the specified entry.
	 * The value can be .
	 */
	function Set(name:String, value:String):Void;
}

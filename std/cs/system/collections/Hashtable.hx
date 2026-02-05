package cs.system.collections;

/** Represents a collection of key/value pairs that are organized based on the hash code of the key. */
@:native("System.Collections.Hashtable")
extern class Hashtable {
	/**
	 * Gets or sets the  to use for the .
	 * @return The  to use for the .
	 */
	var comparer(default, default):cs.system.collections.IComparer;
	/**
	 * Gets the number of key/value pairs contained in the .
	 * @return The number of key/value pairs contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets the  to use for the .
	 * @return The  to use for the .
	 */
	var EqualityComparer(default, never):cs.system.collections.IEqualityComparer;
	/**
	 * Gets or sets the object that can dispense hash codes.
	 * @return The object that can dispense hash codes.
	 */
	var hcp(default, default):cs.system.collections.IHashCodeProvider;
	/**
	 * Gets a value indicating whether the  has a fixed size.
	 * @return if the  has a fixed size; otherwise, . The default is .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return if the  is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return if access to the  is synchronized (thread safe); otherwise, . The
	 * default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an  containing the keys in the .
	 * @return An  containing the keys in the .
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Gets an  containing the values in the .
	 * @return An  containing the values in the .
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:Dynamic):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(d:cs.system.collections.IDictionary):Void {})
	@:overload(function(equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(d:cs.system.collections.IDictionary, equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(d:cs.system.collections.IDictionary, loadFactor:Single):Void {})
	@:overload(function(hcp:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(capacity:Int, equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(capacity:Int, loadFactor:Single):Void {})
	@:overload(function(d:cs.system.collections.IDictionary, hcp:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(d:cs.system.collections.IDictionary, loadFactor:Single, equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(capacity:Int, hcp:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(capacity:Int, loadFactor:Single, equalityComparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(d:cs.system.collections.IDictionary, loadFactor:Single, hcp:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void {})
	function new(capacity:Int, loadFactor:Single, hcp:cs.system.collections.IHashCodeProvider, comparer:cs.system.collections.IComparer):Void;
	/**
	 * Returns a synchronized (thread-safe) wrapper for the .
	 * @param table The  to synchronize.
	 * @return A synchronized (thread-safe) wrapper for the .
	 */
	static function Synchronized(table:cs.system.collections.Hashtable):cs.system.collections.Hashtable;
	/**
	 * Adds an element with the specified key and value into the .
	 * @param key The key of the element to add.
	 * @param value The value of the element to add. The value can be .
	 */
	function Add(key:Dynamic, value:Dynamic):Void;
	/** Removes all elements from the . */
	function Clear():Void;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether the  contains a specific key.
	 * @param key The key to locate in the .
	 * @return if the  contains an element with the specified key; otherwise, .
	 */
	function Contains(key:Dynamic):Bool;
	/**
	 * Determines whether the  contains a specific key.
	 * @param key The key to locate in the .
	 * @return if the  contains an element with the specified key; otherwise, .
	 */
	function ContainsKey(key:Dynamic):Bool;
	/**
	 * Determines whether the  contains a specific value.
	 * @param value The value to locate in the . The value can be .
	 * @return if the  contains an element with the specified ; otherwise, .
	 */
	function ContainsValue(value:Dynamic):Bool;
	/**
	 * Copies the  elements to a one-dimensional  instance at the specified index.
	 * @param array The one-dimensional  that is the destination of the  objects copied
	 * from . The  must have zero-based indexing.
	 * @param arrayIndex The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, arrayIndex:Int):Void;
	/**
	 * Returns an  that iterates through the .
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Implements the  interface and returns the data needed to serialize the .
	 * @param info A  object containing the information required to serialize the .
	 * @param context A  object containing the source and destination of the serialized
	 * stream associated with the .
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Implements the  interface and raises the deserialization event when the
	 * deserialization is complete.
	 * @param sender The source of the deserialization event.
	 */
	function OnDeserialization(sender:Dynamic):Void;
	/**
	 * Removes the element with the specified key from the .
	 * @param key The key of the element to remove.
	 */
	function Remove(key:Dynamic):Void;
}

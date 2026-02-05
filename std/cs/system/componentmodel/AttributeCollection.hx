package cs.system.componentmodel;

/** Represents a collection of attributes. */
@:native("System.ComponentModel.AttributeCollection")
extern class AttributeCollection {
	/** Specifies an empty collection that you can use, rather than creating a new one. This field is read-only. */
	static var Empty(default, never):cs.system.componentmodel.AttributeCollection;
	/**
	 * Gets the attribute collection.
	 * @return The attribute collection.
	 */
	var Attributes(default, never):cs.NativeArray<cs.system.Attribute>;
	/**
	 * Gets the number of attributes.
	 * @return The number of attributes.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.Attribute {})
	@:native("get_Item")
	function get_Item(index0:cs.system.Type):cs.system.Attribute;
	function new(attributes:cs.NativeArray<cs.system.Attribute>):Void;
	/**
	 * Creates a new  from an existing .
	 * @param existing An  from which to create the copy.
	 * @param newAttributes An array of type  that provides the attributes for this
	 * collection. Can be .
	 * @return A new  that is a copy of .
	 */
	static function FromExisting(existing:cs.system.componentmodel.AttributeCollection, newAttributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.AttributeCollection;
	@:overload(function(attribute:cs.system.Attribute):Bool {})
	/**
	 * Determines whether this collection of attributes has the specified attribute.
	 * @param attribute An  to find in the collection.
	 * @return if the collection contains the attribute or is the default attribute for
	 * the type of attribute; otherwise, .
	 */
	function Contains(attributes:cs.NativeArray<cs.system.Attribute>):Bool;
	/**
	 * Copies the collection to an array, starting at the specified index.
	 * @param array The  to copy the collection to.
	 * @param index The index to start from.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Gets an enumerator for this collection.
	 * @return An enumerator of type .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(attribute:cs.system.Attribute):Bool {})
	/**
	 * Determines whether a specified attribute is the same as an attribute in the
	 * collection.
	 * @param attribute An instance of  to compare with the attributes in this
	 * collection.
	 * @return if the attribute is contained within the collection and has the same
	 * value as the attribute in the collection; otherwise, .
	 */
	function Matches(attributes:cs.NativeArray<cs.system.Attribute>):Bool;
}

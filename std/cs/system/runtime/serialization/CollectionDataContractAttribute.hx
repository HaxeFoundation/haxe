package cs.system.runtime.serialization;

/** When applied to a collection type, enables custom specification of the collection item elements. This attribute can be applied only to types that are recognized by the  as valid, serializable collections. */
@:native("System.Runtime.Serialization.CollectionDataContractAttribute")
extern class CollectionDataContractAttribute extends cs.system.Attribute {
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the item name has been explicitly set; otherwise, .
	 */
	var IsItemNameSetExplicitly(default, never):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the key name has been explicitly set; otherwise, .
	 */
	var IsKeyNameSetExplicitly(default, never):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the name has been explicitly set; otherwise, .
	 */
	var IsNameSetExplicitly(default, never):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the item namespace has been explicitly set; otherwise, .
	 */
	var IsNamespaceSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets a value that indicates whether to preserve object reference data.
	 * @return to keep object reference data; otherwise, . The default is .
	 */
	var IsReference(default, default):Bool;
	/**
	 * Gets whether reference has been explicitly set.
	 * @return if the reference has been explicitly set; otherwise, .
	 */
	var IsReferenceSetExplicitly(default, never):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the value name has been explicitly set; otherwise, .
	 */
	var IsValueNameSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets a custom name for a collection element.
	 * @return The name to apply to collection elements.
	 */
	var ItemName(default, default):String;
	/**
	 * Gets or sets the custom name for a dictionary key name.
	 * @return The name to use instead of the default dictionary key name.
	 */
	var KeyName(default, default):String;
	/**
	 * Gets or sets the data contract name for the collection type.
	 * @return The data contract name for the collection type.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the namespace for the data contract.
	 * @return The namespace of the data contract.
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets the custom name for a dictionary value name.
	 * @return The name to use instead of the default dictionary value name.
	 */
	var ValueName(default, default):String;
	function new():Void;
}

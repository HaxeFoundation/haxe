package cs.system.runtime.serialization;

/** Specifies that the type defines or implements a data contract and is serializable by a serializer, such as the . To make their type serializable, type authors must define a data contract for their type. */
@:native("System.Runtime.Serialization.DataContractAttribute")
extern class DataContractAttribute extends cs.system.Attribute {
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the name has been explicitly set; otherwise, .
	 */
	var IsNameSetExplicitly(default, never):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the namespace has been explicitly set; otherwise, .
	 */
	var IsNamespaceSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets a value that indicates whether to preserve object reference data.
	 * @return to keep object reference data using standard XML; otherwise, . The
	 * default is .
	 */
	var IsReference(default, default):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the reference has been explicitly set; otherwise, .
	 */
	var IsReferenceSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets the name of the data contract for the type.
	 * @return The local name of a data contract. The default is the name of the class
	 * that the attribute is applied to.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the namespace for the data contract for the type.
	 * @return The namespace of the contract.
	 */
	var Namespace(default, default):String;
	function new():Void;
}

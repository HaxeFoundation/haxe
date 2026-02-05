package cs.system.runtime.serialization;

/** Specifies the CLR namespace and XML namespace of the data contract. */
@:native("System.Runtime.Serialization.ContractNamespaceAttribute")
extern class ContractNamespaceAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the CLR namespace of the data contract type.
	 * @return The CLR-legal namespace of a type.
	 */
	var ClrNamespace(default, default):String;
	/**
	 * Gets the namespace of the data contract members.
	 * @return The namespace of the data contract members.
	 */
	var ContractNamespace(default, never):String;
	function new(contractNamespace:String):Void;
}

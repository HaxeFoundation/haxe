package cs.system.diagnostics.contracts;

/** Specifies that a separate type contains the code contracts for this type. */
@:native("System.Diagnostics.Contracts.ContractClassAttribute")
extern class ContractClassAttribute extends cs.system.Attribute {
	/**
	 * Gets the type that contains the code contracts for this type.
	 * @return The type that contains the code contracts for this type.
	 */
	var TypeContainingContracts(default, never):cs.system.Type;
	function new(typeContainingContracts:cs.system.Type):Void;
}

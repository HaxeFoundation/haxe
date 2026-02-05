package cs.system.diagnostics.contracts;

/** Specifies that a class is a contract for a type. */
@:native("System.Diagnostics.Contracts.ContractClassForAttribute")
extern class ContractClassForAttribute extends cs.system.Attribute {
	/**
	 * Gets the type that this code contract applies to.
	 * @return The type that this contract applies to.
	 */
	var TypeContractsAreFor(default, never):cs.system.Type;
	function new(typeContractsAreFor:cs.system.Type):Void;
}

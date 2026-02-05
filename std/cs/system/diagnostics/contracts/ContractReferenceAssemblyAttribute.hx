package cs.system.diagnostics.contracts;

/** Specifies that an assembly is a reference assembly that contains contracts. */
@:native("System.Diagnostics.Contracts.ContractReferenceAssemblyAttribute")
extern class ContractReferenceAssemblyAttribute extends cs.system.Attribute {
	function new():Void;
}

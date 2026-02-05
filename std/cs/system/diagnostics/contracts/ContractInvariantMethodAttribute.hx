package cs.system.diagnostics.contracts;

/** Marks a method as being the invariant method for a class. */
@:native("System.Diagnostics.Contracts.ContractInvariantMethodAttribute")
extern class ContractInvariantMethodAttribute extends cs.system.Attribute {
	function new():Void;
}

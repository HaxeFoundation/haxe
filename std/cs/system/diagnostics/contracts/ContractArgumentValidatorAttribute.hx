package cs.system.diagnostics.contracts;

/** Enables the factoring of legacy  code into separate methods for reuse, and provides full control over thrown exceptions and arguments. */
@:native("System.Diagnostics.Contracts.ContractArgumentValidatorAttribute")
extern class ContractArgumentValidatorAttribute extends cs.system.Attribute {
	function new():Void;
}

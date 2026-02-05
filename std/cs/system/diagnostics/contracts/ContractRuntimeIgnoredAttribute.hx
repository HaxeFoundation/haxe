package cs.system.diagnostics.contracts;

/** Identifies a member that has no run-time behavior. */
@:native("System.Diagnostics.Contracts.ContractRuntimeIgnoredAttribute")
extern class ContractRuntimeIgnoredAttribute extends cs.system.Attribute {
	function new():Void;
}

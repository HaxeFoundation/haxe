package cs.system.diagnostics.contracts;

/** Instructs analysis tools to assume the correctness of an assembly, type, or member without performing static verification. */
@:native("System.Diagnostics.Contracts.ContractVerificationAttribute")
extern class ContractVerificationAttribute extends cs.system.Attribute {
	/**
	 * Gets the value that indicates whether to verify the contract of the target.
	 * @return if verification is required; otherwise, .
	 */
	var Value(default, never):Bool;
	function new(value:Bool):Void;
}

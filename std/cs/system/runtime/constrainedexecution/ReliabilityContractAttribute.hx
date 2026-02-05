package cs.system.runtime.constrainedexecution;

/** Defines a contract for reliability between the author of some code, and the developers who have a dependency on that code. */
@:native("System.Runtime.ConstrainedExecution.ReliabilityContractAttribute")
extern class ReliabilityContractAttribute extends cs.system.Attribute {
	/**
	 * Gets the value that determines the behavior of a method, type, or assembly when
	 * called under a Constrained Execution Region (CER).
	 * @return One of the  values.
	 */
	var Cer(default, never):cs.system.runtime.constrainedexecution.Cer;
	/**
	 * Gets the value of the  reliability contract.
	 * @return One of the  values.
	 */
	var ConsistencyGuarantee(default, never):cs.system.runtime.constrainedexecution.Consistency;
	function new(consistencyGuarantee:cs.system.runtime.constrainedexecution.Consistency, cer:cs.system.runtime.constrainedexecution.Cer):Void;
}

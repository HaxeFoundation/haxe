package cs.system.diagnostics.contracts;

/** Specifies the type of contract that failed. */
@:native("System.Diagnostics.Contracts.ContractFailureKind")
extern enum ContractFailureKind {
	Assert;
	Assume;
	Invariant;
	Postcondition;
	PostconditionOnException;
	Precondition;
}

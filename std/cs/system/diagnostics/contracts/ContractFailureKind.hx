package cs.system.diagnostics.contracts;

/** Specifies the type of contract that failed. */
@:native("System.Diagnostics.Contracts.ContractFailureKind")
extern enum abstract ContractFailureKind(Int) {
	var Assert = 4;
	var Assume = 5;
	var Invariant = 3;
	var Postcondition = 1;
	var PostconditionOnException = 2;
	var Precondition = 0;
}

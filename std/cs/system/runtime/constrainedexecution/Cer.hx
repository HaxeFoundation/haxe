package cs.system.runtime.constrainedexecution;

/** Specifies a method's behavior when called within a constrained execution region. */
@:native("System.Runtime.ConstrainedExecution.Cer")
extern enum abstract Cer(Int) {
	var MayFail = 1;
	var None = 0;
	var Success = 2;
}

package cs.system.runtime.constrainedexecution;

/** Specifies a method's behavior when called within a constrained execution region. */
@:native("System.Runtime.ConstrainedExecution.Cer")
extern enum Cer {
	MayFail;
	None;
	Success;
}

package cs.system.runtime.constrainedexecution;

/** Specifies a reliability contract. */
@:native("System.Runtime.ConstrainedExecution.Consistency")
extern enum Consistency {
	MayCorruptAppDomain;
	MayCorruptInstance;
	MayCorruptProcess;
	WillNotCorruptState;
}

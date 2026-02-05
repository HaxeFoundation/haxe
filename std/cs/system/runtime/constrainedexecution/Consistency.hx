package cs.system.runtime.constrainedexecution;

/** Specifies a reliability contract. */
@:native("System.Runtime.ConstrainedExecution.Consistency")
extern enum abstract Consistency(Int) {
	var MayCorruptAppDomain = 1;
	var MayCorruptInstance = 2;
	var MayCorruptProcess = 0;
	var WillNotCorruptState = 3;
}

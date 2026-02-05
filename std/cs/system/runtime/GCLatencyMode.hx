package cs.system.runtime;

/** Adjusts the time that the garbage collector intrudes in your application. */
@:native("System.Runtime.GCLatencyMode")
extern enum abstract GCLatencyMode(Int) {
	var Batch = 0;
	var Interactive = 1;
	var LowLatency = 2;
	var NoGCRegion = 4;
	var SustainedLowLatency = 3;
}

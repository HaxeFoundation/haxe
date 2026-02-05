package cs.system.runtime;

/** Adjusts the time that the garbage collector intrudes in your application. */
@:native("System.Runtime.GCLatencyMode")
extern enum GCLatencyMode {
	Batch;
	Interactive;
	LowLatency;
	NoGCRegion;
	SustainedLowLatency;
}

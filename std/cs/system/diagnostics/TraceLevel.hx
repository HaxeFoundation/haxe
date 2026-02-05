package cs.system.diagnostics;

/** Specifies what messages to output for the ,  and  classes. */
@:native("System.Diagnostics.TraceLevel")
extern enum abstract TraceLevel(Int) {
	var Error = 1;
	var Info = 3;
	var Off = 0;
	var Verbose = 4;
	var Warning = 2;
}

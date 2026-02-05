package cs.system.diagnostics;

/** Specifies what messages to output for the ,  and  classes. */
@:native("System.Diagnostics.TraceLevel")
extern enum TraceLevel {
	Error;
	Info;
	Off;
	Verbose;
	Warning;
}

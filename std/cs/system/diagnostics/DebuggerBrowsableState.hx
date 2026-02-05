package cs.system.diagnostics;

/** Provides display instructions for the debugger. */
@:native("System.Diagnostics.DebuggerBrowsableState")
extern enum abstract DebuggerBrowsableState(Int) {
	var Collapsed = 2;
	var Never = 0;
	var RootHidden = 3;
}

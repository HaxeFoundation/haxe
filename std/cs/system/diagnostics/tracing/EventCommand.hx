package cs.system.diagnostics.tracing;

/** Describes the command ( property) that is passed to the  callback. */
@:native("System.Diagnostics.Tracing.EventCommand")
extern enum abstract EventCommand(Int) {
	var Disable = -3;
	var Enable = -2;
	var SendManifest = -1;
	var Update = 0;
}

package cs.system.diagnostics.tracing;

/** Describes the command ( property) that is passed to the  callback. */
@:native("System.Diagnostics.Tracing.EventCommand")
extern enum EventCommand {
	Disable;
	Enable;
	SendManifest;
	Update;
}

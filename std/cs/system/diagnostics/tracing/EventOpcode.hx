package cs.system.diagnostics.tracing;

/** Defines the standard operation codes that the event source attaches to events. */
@:native("System.Diagnostics.Tracing.EventOpcode")
extern enum EventOpcode {
	DataCollectionStart;
	DataCollectionStop;
	Extension;
	Info;
	Receive;
	Reply;
	Resume;
	Send;
	Start;
	Stop;
	Suspend;
}

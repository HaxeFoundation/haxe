package cs.system.diagnostics.tracing;

/** Defines the standard operation codes that the event source attaches to events. */
@:native("System.Diagnostics.Tracing.EventOpcode")
extern enum abstract EventOpcode(Int) {
	var DataCollectionStart = 3;
	var DataCollectionStop = 4;
	var Extension = 5;
	var Info = 0;
	var Receive = 240;
	var Reply = 6;
	var Resume = 7;
	var Send = 9;
	var Start = 1;
	var Stop = 2;
	var Suspend = 8;
}

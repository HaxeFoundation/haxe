package cs.system.buffers;

/** Defines the values that can be returned from span-based operations that support processing of input contained in multiple discontiguous buffers. */
@:native("System.Buffers.OperationStatus")
extern enum abstract OperationStatus(Int) {
	var DestinationTooSmall = 1;
	var Done = 0;
	var InvalidData = 3;
	var NeedMoreData = 2;
}

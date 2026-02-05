package cs.system.buffers;

/** Defines the values that can be returned from span-based operations that support processing of input contained in multiple discontiguous buffers. */
@:native("System.Buffers.OperationStatus")
extern enum OperationStatus {
	DestinationTooSmall;
	Done;
	InvalidData;
	NeedMoreData;
}

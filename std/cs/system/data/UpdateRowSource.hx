package cs.system.data;

/** Specifies how query command results are applied to the row being updated. */
@:native("System.Data.UpdateRowSource")
extern enum UpdateRowSource {
	Both;
	FirstReturnedRecord;
	None;
	OutputParameters;
}

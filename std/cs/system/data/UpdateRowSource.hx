package cs.system.data;

/** Specifies how query command results are applied to the row being updated. */
@:native("System.Data.UpdateRowSource")
extern enum abstract UpdateRowSource(Int) {
	var Both = 3;
	var FirstReturnedRecord = 2;
	var None = 0;
	var OutputParameters = 1;
}

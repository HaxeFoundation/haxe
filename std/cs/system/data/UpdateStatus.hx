package cs.system.data;

/** Specifies the action to take with regard to the current and remaining rows during an . */
@:native("System.Data.UpdateStatus")
extern enum abstract UpdateStatus(Int) {
	var Continue = 0;
	var ErrorsOccurred = 1;
	var SkipAllRemainingRows = 3;
	var SkipCurrentRow = 2;
}

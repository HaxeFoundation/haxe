package cs.system.data;

/** Specifies the action to take with regard to the current and remaining rows during an . */
@:native("System.Data.UpdateStatus")
extern enum UpdateStatus {
	Continue;
	ErrorsOccurred;
	SkipAllRemainingRows;
	SkipCurrentRow;
}

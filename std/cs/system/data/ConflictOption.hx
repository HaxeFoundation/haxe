package cs.system.data;

/** Specifies how conflicting changes to the data source will be detected and resolved. */
@:native("System.Data.ConflictOption")
extern enum abstract ConflictOption(Int) {
	var CompareAllSearchableValues = 1;
	var CompareRowVersion = 2;
	var OverwriteChanges = 3;
}

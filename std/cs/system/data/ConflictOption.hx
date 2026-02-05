package cs.system.data;

/** Specifies how conflicting changes to the data source will be detected and resolved. */
@:native("System.Data.ConflictOption")
extern enum ConflictOption {
	CompareAllSearchableValues;
	CompareRowVersion;
	OverwriteChanges;
}

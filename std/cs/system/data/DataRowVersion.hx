package cs.system.data;

/** Describes the version of a . */
@:native("System.Data.DataRowVersion")
extern enum DataRowVersion {
	Current;
	Default;
	Original;
	Proposed;
}

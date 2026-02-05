package cs.system.data;

/** Describes the version of a . */
@:native("System.Data.DataRowVersion")
extern enum abstract DataRowVersion(Int) {
	var Current = 512;
	var Default = 1536;
	var Original = 256;
	var Proposed = 1024;
}

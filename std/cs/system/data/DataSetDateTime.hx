package cs.system.data;

/** Describes the serialization format for  columns in a . */
@:native("System.Data.DataSetDateTime")
extern enum abstract DataSetDateTime(Int) {
	var Local = 1;
	var Unspecified = 2;
	var UnspecifiedLocal = 3;
	var Utc = 4;
}

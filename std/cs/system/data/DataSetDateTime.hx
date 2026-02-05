package cs.system.data;

/** Describes the serialization format for  columns in a . */
@:native("System.Data.DataSetDateTime")
extern enum DataSetDateTime {
	Local;
	Unspecified;
	UnspecifiedLocal;
	Utc;
}

package cs.system.data.common;

/** Specifies how identifiers are treated by the data source when searching the system catalog. */
@:native("System.Data.Common.IdentifierCase")
extern enum abstract IdentifierCase(Int) {
	var Insensitive = 1;
	var Sensitive = 2;
	var Unknown = 0;
}

package cs.system.data.common;

/** Specifies how identifiers are treated by the data source when searching the system catalog. */
@:native("System.Data.Common.IdentifierCase")
extern enum IdentifierCase {
	Insensitive;
	Sensitive;
	Unknown;
}

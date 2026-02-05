package cs.system.data.sqltypes;

/** All the  objects and structures implement the  interface. */
@:native("System.Data.SqlTypes.INullable")
extern interface INullable {
	/**
	 * Indicates whether a structure is null. This property is read-only.
	 * @return if the value of this object is null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
}

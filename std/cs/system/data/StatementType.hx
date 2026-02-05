package cs.system.data;

/** Specifies the type of SQL query to be used by the , , , or  class. */
@:native("System.Data.StatementType")
extern enum abstract StatementType(Int) {
	var Batch = 4;
	var Delete = 3;
	var Insert = 1;
	var Select = 0;
	var Update = 2;
}

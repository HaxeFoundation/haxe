package cs.system.data;

/** Specifies how a command string is interpreted. */
@:native("System.Data.CommandType")
extern enum abstract CommandType(Int) {
	var StoredProcedure = 4;
	var TableDirect = 512;
	var Text = 1;
}

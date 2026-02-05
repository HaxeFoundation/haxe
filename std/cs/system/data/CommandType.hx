package cs.system.data;

/** Specifies how a command string is interpreted. */
@:native("System.Data.CommandType")
extern enum CommandType {
	StoredProcedure;
	TableDirect;
	Text;
}

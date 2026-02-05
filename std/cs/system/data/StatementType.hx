package cs.system.data;

/** Specifies the type of SQL query to be used by the , , , or  class. */
@:native("System.Data.StatementType")
extern enum StatementType {
	Batch;
	Delete;
	Insert;
	Select;
	Update;
}

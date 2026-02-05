package cs.system.data;

/** Specifies the action to take when adding data to the  and the required  or  is missing. */
@:native("System.Data.MissingSchemaAction")
extern enum MissingSchemaAction {
	Add;
	AddWithKey;
	Error;
	Ignore;
}

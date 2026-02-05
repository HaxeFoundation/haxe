package cs.system.data;

/** Specifies the action to take when adding data to the  and the required  or  is missing. */
@:native("System.Data.MissingSchemaAction")
extern enum abstract MissingSchemaAction(Int) {
	var Add = 1;
	var AddWithKey = 4;
	var Error = 3;
	var Ignore = 2;
}

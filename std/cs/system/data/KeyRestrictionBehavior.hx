package cs.system.data;

/** Identifies a list of connection string parameters identified by the  property that are either allowed or not allowed. */
@:native("System.Data.KeyRestrictionBehavior")
extern enum abstract KeyRestrictionBehavior(Int) {
	var AllowOnly = 0;
	var PreventUsage = 1;
}

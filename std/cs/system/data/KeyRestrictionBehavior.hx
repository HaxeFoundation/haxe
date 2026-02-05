package cs.system.data;

/** Identifies a list of connection string parameters identified by the  property that are either allowed or not allowed. */
@:native("System.Data.KeyRestrictionBehavior")
extern enum KeyRestrictionBehavior {
	AllowOnly;
	PreventUsage;
}

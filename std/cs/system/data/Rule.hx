package cs.system.data;

/** Indicates the action that occurs when a  is enforced. */
@:native("System.Data.Rule")
extern enum Rule {
	Cascade;
	None;
	SetDefault;
	SetNull;
}

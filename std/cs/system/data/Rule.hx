package cs.system.data;

/** Indicates the action that occurs when a  is enforced. */
@:native("System.Data.Rule")
extern enum abstract Rule(Int) {
	var Cascade = 1;
	var None = 0;
	var SetDefault = 3;
	var SetNull = 2;
}

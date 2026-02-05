package cs.system.data;

/** Determines the action that occurs when the  or  method is invoked on a  with a . */
@:native("System.Data.AcceptRejectRule")
extern enum abstract AcceptRejectRule(Int) {
	var Cascade = 1;
	var None = 0;
}

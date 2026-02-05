package cs.system.collections.specialized;

/** Describes the action that caused a  event. */
@:native("System.Collections.Specialized.NotifyCollectionChangedAction")
extern enum abstract NotifyCollectionChangedAction(Int) {
	var Add = 0;
	var Move = 3;
	var Remove = 1;
	var Replace = 2;
	var Reset = 4;
}

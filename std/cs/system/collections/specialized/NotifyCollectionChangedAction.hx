package cs.system.collections.specialized;

/** Describes the action that caused a  event. */
@:native("System.Collections.Specialized.NotifyCollectionChangedAction")
extern enum NotifyCollectionChangedAction {
	Add;
	Move;
	Remove;
	Replace;
	Reset;
}

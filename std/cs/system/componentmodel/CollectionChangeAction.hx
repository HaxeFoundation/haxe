package cs.system.componentmodel;

/** Specifies how the collection is changed. */
@:native("System.ComponentModel.CollectionChangeAction")
extern enum CollectionChangeAction {
	Add;
	Refresh;
	Remove;
}

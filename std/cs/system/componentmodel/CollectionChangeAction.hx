package cs.system.componentmodel;

/** Specifies how the collection is changed. */
@:native("System.ComponentModel.CollectionChangeAction")
extern enum abstract CollectionChangeAction(Int) {
	var Add = 1;
	var Refresh = 3;
	var Remove = 2;
}

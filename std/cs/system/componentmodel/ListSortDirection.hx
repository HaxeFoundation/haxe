package cs.system.componentmodel;

/** Specifies the direction of a sort operation. */
@:native("System.ComponentModel.ListSortDirection")
extern enum abstract ListSortDirection(Int) {
	var Ascending = 0;
	var Descending = 1;
}

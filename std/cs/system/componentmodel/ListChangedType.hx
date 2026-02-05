package cs.system.componentmodel;

/** Specifies how the list changed. */
@:native("System.ComponentModel.ListChangedType")
extern enum abstract ListChangedType(Int) {
	var ItemAdded = 1;
	var ItemChanged = 4;
	var ItemDeleted = 2;
	var ItemMoved = 3;
	var PropertyDescriptorAdded = 5;
	var PropertyDescriptorChanged = 7;
	var PropertyDescriptorDeleted = 6;
	var Reset = 0;
}

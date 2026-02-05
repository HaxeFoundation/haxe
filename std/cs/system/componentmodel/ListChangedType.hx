package cs.system.componentmodel;

/** Specifies how the list changed. */
@:native("System.ComponentModel.ListChangedType")
extern enum ListChangedType {
	ItemAdded;
	ItemChanged;
	ItemDeleted;
	ItemMoved;
	PropertyDescriptorAdded;
	PropertyDescriptorChanged;
	PropertyDescriptorDeleted;
	Reset;
}

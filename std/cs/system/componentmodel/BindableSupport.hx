package cs.system.componentmodel;

/** Specifies values to indicate whether a property can be bound to a data element or another property. */
@:native("System.ComponentModel.BindableSupport")
extern enum abstract BindableSupport(Int) {
	var Default = 2;
	var No = 0;
	var Yes = 1;
}

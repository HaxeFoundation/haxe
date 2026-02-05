package cs.system.componentmodel;

/** Specifies whether the template can be bound one way or two ways. */
@:native("System.ComponentModel.BindingDirection")
extern enum abstract BindingDirection(Int) {
	var OneWay = 0;
	var TwoWay = 1;
}

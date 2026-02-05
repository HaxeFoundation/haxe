package cs.system.componentmodel;

/** Defines identifiers for types of inheritance levels. */
@:native("System.ComponentModel.InheritanceLevel")
extern enum abstract InheritanceLevel(Int) {
	var Inherited = 1;
	var InheritedReadOnly = 2;
	var NotInherited = 3;
}

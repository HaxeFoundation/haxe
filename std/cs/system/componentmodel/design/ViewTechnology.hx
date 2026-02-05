package cs.system.componentmodel.design;

/** Defines identifiers for a set of technologies that designer hosts support. */
@:native("System.ComponentModel.Design.ViewTechnology")
extern enum abstract ViewTechnology(Int) {
	var Default = 2;
	var Passthrough = 0;
	var WindowsForms = 1;
}

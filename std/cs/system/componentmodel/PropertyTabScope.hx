package cs.system.componentmodel;

/** Defines identifiers that indicate the persistence scope of a tab in the Properties window. */
@:native("System.ComponentModel.PropertyTabScope")
extern enum abstract PropertyTabScope(Int) {
	var Component = 3;
	var Document = 2;
	var Global = 1;
	var Static = 0;
}

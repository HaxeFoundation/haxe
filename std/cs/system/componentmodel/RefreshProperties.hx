package cs.system.componentmodel;

/** Defines identifiers that indicate the type of a refresh of the Properties window. */
@:native("System.ComponentModel.RefreshProperties")
extern enum abstract RefreshProperties(Int) {
	var All = 1;
	var None = 0;
	var Repaint = 2;
}

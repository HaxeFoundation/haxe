package cs.system.componentmodel;

/** Defines identifiers used to indicate the type of filter that a  uses. */
@:native("System.ComponentModel.ToolboxItemFilterType")
extern enum abstract ToolboxItemFilterType(Int) {
	var Allow = 0;
	var Custom = 1;
	var Prevent = 2;
	var Require = 3;
}

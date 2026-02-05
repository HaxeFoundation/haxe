package cs.system.componentmodel;

/** Defines identifiers used to indicate the type of filter that a  uses. */
@:native("System.ComponentModel.ToolboxItemFilterType")
extern enum ToolboxItemFilterType {
	Allow;
	Custom;
	Prevent;
	Require;
}

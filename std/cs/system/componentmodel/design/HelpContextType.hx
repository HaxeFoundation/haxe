package cs.system.componentmodel.design;

/** Defines identifiers that indicate information about the context in which a request for Help information originated. */
@:native("System.ComponentModel.Design.HelpContextType")
extern enum abstract HelpContextType(Int) {
	var Ambient = 0;
	var Selection = 2;
	var ToolWindowSelection = 3;
	var Window = 1;
}

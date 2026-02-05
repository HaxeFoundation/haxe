package cs.system.componentmodel.design;

/** Defines identifiers that indicate information about the context in which a request for Help information originated. */
@:native("System.ComponentModel.Design.HelpContextType")
extern enum HelpContextType {
	Ambient;
	Selection;
	ToolWindowSelection;
	Window;
}

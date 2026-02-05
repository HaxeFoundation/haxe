package cs.system.componentmodel.design;

/** Provides data for the  event. */
@:native("System.ComponentModel.Design.ActiveDesignerEventArgs")
extern class ActiveDesignerEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the document that is gaining activation.
	 * @return An  that represents the document gaining activation.
	 */
	var NewDesigner(default, never):cs.system.componentmodel.design.IDesignerHost;
	/**
	 * Gets the document that is losing activation.
	 * @return An  that represents the document losing activation.
	 */
	var OldDesigner(default, never):cs.system.componentmodel.design.IDesignerHost;
	function new(oldDesigner:cs.system.componentmodel.design.IDesignerHost, newDesigner:cs.system.componentmodel.design.IDesignerHost):Void;
}

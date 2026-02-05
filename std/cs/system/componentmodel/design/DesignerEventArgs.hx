package cs.system.componentmodel.design;

/** Provides data for the  and  events. */
@:native("System.ComponentModel.Design.DesignerEventArgs")
extern class DesignerEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the host of the document.
	 * @return The  of the document.
	 */
	var Designer(default, never):cs.system.componentmodel.design.IDesignerHost;
	function new(host:cs.system.componentmodel.design.IDesignerHost):Void;
}

package cs.system.componentmodel.design;

/**
 * Represents the method that handles the  and  events of a designer.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.Design.DesignerTransactionCloseEventHandler")
extern class DesignerTransactionCloseEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.DesignerTransactionCloseEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.DesignerTransactionCloseEventArgs):Void;
}

package cs.system.componentmodel.design;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e An  that contains the event data.
 */
@:native("System.ComponentModel.Design.ActiveDesignerEventHandler")
extern class ActiveDesignerEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.ActiveDesignerEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.ActiveDesignerEventArgs):Void;
}

package cs.system.componentmodel.design;

/**
 * Represents the method that will handle the  and  events that are raised when a
 * document is created or disposed of.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.Design.DesignerEventHandler")
extern class DesignerEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.DesignerEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.DesignerEventArgs):Void;
}

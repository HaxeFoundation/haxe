package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event. This class cannot be
 * inherited.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.DoWorkEventHandler")
extern class DoWorkEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.DoWorkEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.DoWorkEventArgs):Void;
}

package cs.system.componentmodel.design.serialization;

/**
 * Represents the method that handles the  event of a serialization manager.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.Design.Serialization.ResolveNameEventHandler")
extern class ResolveNameEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.serialization.ResolveNameEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.serialization.ResolveNameEventArgs):Void;
}

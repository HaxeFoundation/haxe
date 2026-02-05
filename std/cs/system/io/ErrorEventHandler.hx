package cs.system.io;

/**
 * Represents the method that will handle the  event of a  object.
 * @param sender The source of the event.
 * @param e An  object that contains the event data.
 */
@:native("System.IO.ErrorEventHandler")
extern class ErrorEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.io.ErrorEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.io.ErrorEventArgs):Void;
}

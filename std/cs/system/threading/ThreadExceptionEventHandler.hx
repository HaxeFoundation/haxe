package cs.system.threading;

/**
 * Represents the method that will handle the  event of an .
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Threading.ThreadExceptionEventHandler")
extern class ThreadExceptionEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.threading.ThreadExceptionEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.threading.ThreadExceptionEventArgs):Void;
}

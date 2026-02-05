package cs.system;

/**
 * Represents the method that will handle the event raised by an exception that is
 * not handled by the application domain.
 * @param sender The source of the unhandled exception event.
 * @param e An UnhandledExceptionEventArgs that contains the event data.
 */
@:native("System.UnhandledExceptionEventHandler")
extern class UnhandledExceptionEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.UnhandledExceptionEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.UnhandledExceptionEventArgs):Void;
}

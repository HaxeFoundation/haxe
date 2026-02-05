package cs.system.net.mail;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e An  containing event data.
 */
@:native("System.Net.Mail.SendCompletedEventHandler")
extern class SendCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.AsyncCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.AsyncCompletedEventArgs):Void;
}

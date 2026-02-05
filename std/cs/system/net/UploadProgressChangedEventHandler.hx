package cs.system.net;

/**
 * Represents the method that will handle the  event of a .
 * @param sender The source of the event.
 * @param e A  containing event data.
 */
@:native("System.Net.UploadProgressChangedEventHandler")
extern class UploadProgressChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.net.UploadProgressChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.net.UploadProgressChangedEventArgs):Void;
}

package cs.system.net;

/**
 * Represents the method that will handle the  event of a .
 * @param sender The source of the event.
 * @param e A  that contains event data.
 */
@:native("System.Net.DownloadStringCompletedEventHandler")
extern class DownloadStringCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.net.DownloadStringCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.net.DownloadStringCompletedEventArgs):Void;
}

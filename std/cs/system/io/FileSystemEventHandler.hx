package cs.system.io;

/**
 * Represents the method that will handle the , , or  event of a  class.
 * @param sender The source of the event.
 * @param e The  that contains the event data.
 */
@:native("System.IO.FileSystemEventHandler")
extern class FileSystemEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.io.FileSystemEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.io.FileSystemEventArgs):Void;
}

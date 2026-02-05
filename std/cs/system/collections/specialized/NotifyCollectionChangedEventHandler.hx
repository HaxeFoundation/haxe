package cs.system.collections.specialized;

/**
 * Represents the method that handles the  event.
 * @param sender The object that raised the event.
 * @param e Information about the event.
 */
@:native("System.Collections.Specialized.NotifyCollectionChangedEventHandler")
extern class NotifyCollectionChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.collections.specialized.NotifyCollectionChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.collections.specialized.NotifyCollectionChangedEventArgs):Void;
}

package cs.system.xml;

/**
 * Represents the method that handles , , , ,  and  events.
 * @param sender The source of the event.
 * @param e An  containing the event data.
 */
@:native("System.Xml.XmlNodeChangedEventHandler")
extern class XmlNodeChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.XmlNodeChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.XmlNodeChangedEventArgs):Void;
}

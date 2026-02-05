package cs.system.xml.serialization;

/**
 * Represents the method that handles the  event of an .
 * @param sender The source of the event.
 * @param e An  that contains the event data.
 */
@:native("System.Xml.Serialization.XmlNodeEventHandler")
extern class XmlNodeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.serialization.XmlNodeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.serialization.XmlNodeEventArgs):Void;
}

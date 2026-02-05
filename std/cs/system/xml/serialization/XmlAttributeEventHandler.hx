package cs.system.xml.serialization;

/**
 * Represents the method that handles the
 * @param sender The source of the event.
 * @param e An  that contains the event data.
 */
@:native("System.Xml.Serialization.XmlAttributeEventHandler")
extern class XmlAttributeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.serialization.XmlAttributeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.serialization.XmlAttributeEventArgs):Void;
}

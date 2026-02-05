package cs.system.xml.serialization;

/**
 * Represents the method that handles the  event of an .
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Xml.Serialization.XmlElementEventHandler")
extern class XmlElementEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.serialization.XmlElementEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.serialization.XmlElementEventArgs):Void;
}

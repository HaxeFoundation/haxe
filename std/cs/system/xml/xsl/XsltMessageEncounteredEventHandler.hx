package cs.system.xml.xsl;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e The  containing the event data.
 */
@:native("System.Xml.Xsl.XsltMessageEncounteredEventHandler")
extern class XsltMessageEncounteredEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.xsl.XsltMessageEncounteredEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.xsl.XsltMessageEncounteredEventArgs):Void;
}

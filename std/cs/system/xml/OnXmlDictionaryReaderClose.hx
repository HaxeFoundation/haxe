package cs.system.xml;

/**
 * for a callback method when closing the reader.
 * @param reader The  that fires the OnClose event.
 */
@:native("System.Xml.OnXmlDictionaryReaderClose")
extern class OnXmlDictionaryReaderClose extends cs.system.MulticastDelegate {
	function new(func:(reader:cs.system.xml.XmlDictionaryReader)->Void):Void;
	function Invoke(reader:cs.system.xml.XmlDictionaryReader):Void;
}

package cs.system.xml;

/** Enables a class to return an  from the current context or position. */
@:native("System.Xml.IHasXmlNode")
extern interface IHasXmlNode {
	/**
	 * Returns the  for the current position.
	 * @return The  for the current position.
	 */
	function GetNode():cs.system.xml.XmlNode;
}

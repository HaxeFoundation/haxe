package cs.system.xml.xpath;

/** Provides an accessor to the  class. */
@:native("System.Xml.XPath.IXPathNavigable")
extern interface IXPathNavigable {
	/**
	 * Returns a new  object.
	 * @return An  object.
	 */
	function CreateNavigator():cs.system.xml.xpath.XPathNavigator;
}

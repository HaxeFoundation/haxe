package cs.system.xml.xpath;

/** Extends the  class by providing a method for navigating and editing an XML node. */
@:native("System.Xml.XPath.XDocumentExtensions")
extern class XDocumentExtensions {
	/**
	 * Returns an accessor that allows you to navigate and edit the specified .
	 * @param node The XML node to navigate.
	 * @return An interface that provides an accessor to the  class.
	 */
	static function ToXPathNavigable(node:cs.system.xml.linq.XNode):cs.system.xml.xpath.IXPathNavigable;
}

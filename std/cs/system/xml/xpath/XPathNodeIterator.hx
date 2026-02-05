package cs.system.xml.xpath;

/** Provides an iterator over a selected set of nodes. */
@:native("System.Xml.XPath.XPathNodeIterator")
extern class XPathNodeIterator {
	/**
	 * Gets the index of the last node in the selected set of nodes.
	 * @return The index of the last node in the selected set of nodes, or 0 if there
	 * are no selected nodes.
	 */
	var Count(default, never):Int;
	/**
	 * When overridden in a derived class, gets the  object for this , positioned on
	 * the current context node.
	 * @return An  object positioned on the context node from which the node set was
	 * selected. The  method must be called to move the  to the first node in the
	 * selected set.
	 */
	var Current(default, never):cs.system.xml.xpath.XPathNavigator;
	/**
	 * When overridden in a derived class, gets the index of the current position in
	 * the selected set of nodes.
	 * @return The index of the current position.
	 */
	var CurrentPosition(default, never):Int;
	/**
	 * When overridden in a derived class, returns a clone of this  object.
	 * @return A new  object clone of this  object.
	 */
	function Clone():cs.system.xml.xpath.XPathNodeIterator;
	/**
	 * Returns an  object to iterate through the selected node set.
	 * @return An  object to iterate through the selected node set.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * When overridden in a derived class, moves the  object returned by the  property
	 * to the next node in the selected node set.
	 * @return if the  object moved to the next node;  if there are no more selected
	 * nodes.
	 */
	function MoveNext():Bool;
}

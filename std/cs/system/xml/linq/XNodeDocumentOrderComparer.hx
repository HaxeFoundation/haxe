package cs.system.xml.linq;

/** Contains functionality to compare nodes for their document order. This class cannot be inherited. */
@:native("System.Xml.Linq.XNodeDocumentOrderComparer")
extern class XNodeDocumentOrderComparer {
	function new():Void;
	/**
	 * Compares two nodes to determine their relative document order.
	 * @param x The first  to compare.
	 * @param y The second  to compare.
	 * @return An  that contains 0 if the nodes are equal; -1 if  is before ; 1 if  is
	 * after .
	 */
	function Compare(x:cs.system.xml.linq.XNode, y:cs.system.xml.linq.XNode):Int;
}

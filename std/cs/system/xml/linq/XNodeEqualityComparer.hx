package cs.system.xml.linq;

/** Compares nodes to determine whether they are equal. This class cannot be inherited. */
@:native("System.Xml.Linq.XNodeEqualityComparer")
extern class XNodeEqualityComparer {
	function new():Void;
	/**
	 * Compares the values of two nodes.
	 * @param x The first  to compare.
	 * @param y The second  to compare.
	 * @return A  indicating if the nodes are equal.
	 */
	function Equals(x:cs.system.xml.linq.XNode, y:cs.system.xml.linq.XNode):Bool;
	/**
	 * Returns a hash code based on an .
	 * @param obj The  to hash.
	 * @return A  that contains a value-based hash code for the node.
	 */
	function GetHashCode(obj:cs.system.xml.linq.XNode):Int;
}

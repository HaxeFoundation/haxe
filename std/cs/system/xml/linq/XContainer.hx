package cs.system.xml.linq;

/** Represents a node that can contain other nodes. */
@:native("System.Xml.Linq.XContainer")
extern class XContainer extends cs.system.xml.linq.XNode {
	/**
	 * Gets the first child node of this node.
	 * @return An  containing the first child node of the .
	 */
	var FirstNode(default, never):cs.system.xml.linq.XNode;
	/**
	 * Gets the last child node of this node.
	 * @return An  containing the last child node of the .
	 */
	var LastNode(default, never):cs.system.xml.linq.XNode;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Adds the specified content as children of this .
	 * @param content A content object containing simple content or a collection of
	 * content objects to be added.
	 */
	function Add(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Adds the specified content as the first children of this document or element.
	 * @param content A content object containing simple content or a collection of
	 * content objects to be added.
	 */
	function AddFirst(content:cs.NativeArray<Dynamic>):Void;
	/**
	 * Creates an  that can be used to add nodes to the .
	 * @return An  that is ready to have content written to it.
	 */
	function CreateWriter():cs.system.xml.XmlWriter;
	/**
	 * Returns a collection of the descendant nodes for this document or element, in
	 * document order.
	 * @return An  of  containing the descendant nodes of the , in document order.
	 */
	function DescendantNodes():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the descendant elements for this document or element, in
	 * document order.
	 * @return An  of  containing the descendant elements of the .
	 */
	function Descendants(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Gets the first (in document order) child element with the specified .
	 * @param name The  to match.
	 * @return A  that matches the specified , or .
	 */
	function Element(name:cs.system.xml.linq.XName):cs.system.xml.linq.XElement;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the child elements of this element or document, in
	 * document order.
	 * @return An  of  containing the child elements of this , in document order.
	 */
	function Elements(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Returns a collection of the child nodes of this element or document, in document
	 * order.
	 * @return An  of  containing the contents of this , in document order.
	 */
	function Nodes():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	/** Removes the child nodes from this document or element. */
	function RemoveNodes():Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Replaces the children nodes of this document or element with the specified
	 * content.
	 * @param content A content object containing simple content or a collection of
	 * content objects that replace the children nodes.
	 */
	function ReplaceNodes(content:cs.NativeArray<Dynamic>):Void;
}

package cs.system.xml.linq;

/** Represents the abstract concept of a node (element, comment, document type, processing instruction, or text node) in the XML tree. */
@:native("System.Xml.Linq.XNode")
extern class XNode extends cs.system.xml.linq.XObject {
	/**
	 * Gets a comparer that can compare the relative position of two nodes.
	 * @return An  that can compare the relative position of two nodes.
	 */
	static var DocumentOrderComparer(default, never):cs.system.xml.linq.XNodeDocumentOrderComparer;
	/**
	 * Gets a comparer that can compare two nodes for value equality.
	 * @return A  that can compare two nodes for value equality.
	 */
	static var EqualityComparer(default, never):cs.system.xml.linq.XNodeEqualityComparer;
	/**
	 * Gets the next sibling node of this node.
	 * @return The  that contains the next sibling node.
	 */
	var NextNode(default, never):cs.system.xml.linq.XNode;
	/**
	 * Gets the previous sibling node of this node.
	 * @return The  that contains the previous sibling node.
	 */
	var PreviousNode(default, never):cs.system.xml.linq.XNode;
	/**
	 * Compares two nodes to determine their relative XML document order.
	 * @param n1 First  to compare.
	 * @param n2 Second  to compare.
	 * @return An  containing 0 if the nodes are equal; -1 if  is before ; 1 if  is
	 * after .
	 */
	static function CompareDocumentOrder(n1:cs.system.xml.linq.XNode, n2:cs.system.xml.linq.XNode):Int;
	/**
	 * Compares the values of two nodes, including the values of all descendant nodes.
	 * @param n1 The first  to compare.
	 * @param n2 The second  to compare.
	 * @return if the nodes are equal; otherwise .
	 */
	static function DeepEquals(n1:cs.system.xml.linq.XNode, n2:cs.system.xml.linq.XNode):Bool;
	/**
	 * Creates an  from an .
	 * @param reader An  positioned at the node to read into this .
	 * @return An  that contains the node and its descendant nodes that were read from
	 * the reader. The runtime type of the node is determined by the node type () of
	 * the first node encountered in the reader.
	 */
	static function ReadFrom(reader:cs.system.xml.XmlReader):cs.system.xml.linq.XNode;
	/**
	 * @param reader 
	 * @param cancellationToken 
	 */
	static function ReadFromAsync(reader:cs.system.xml.XmlReader, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XNode>;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Adds the specified content immediately after this node.
	 * @param content A content object that contains simple content or a collection of
	 * content objects to be added after this node.
	 */
	function AddAfterSelf(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Adds the specified content immediately before this node.
	 * @param content A content object that contains simple content or a collection of
	 * content objects to be added before this node.
	 */
	function AddBeforeSelf(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the ancestor elements of this node.
	 * @return An  of  of the ancestor elements of this node.
	 */
	function Ancestors(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function():cs.system.xml.XmlReader {})
	/**
	 * Creates an  for this node.
	 * @return An  that can be used to read this node and its descendants.
	 */
	function CreateReader(readerOptions:cs.system.xml.linq.ReaderOptions):cs.system.xml.XmlReader;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the sibling elements after this node, in document order.
	 * @return An  of  of the sibling elements after this node, in document order.
	 */
	function ElementsAfterSelf(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the sibling elements before this node, in document
	 * order.
	 * @return An  of  of the sibling elements before this node, in document order.
	 */
	function ElementsBeforeSelf(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Determines if the current node appears after a specified node in terms of
	 * document order.
	 * @param node The  to compare for document order.
	 * @return if this node appears after the specified node; otherwise .
	 */
	function IsAfter(node:cs.system.xml.linq.XNode):Bool;
	/**
	 * Determines if the current node appears before a specified node in terms of
	 * document order.
	 * @param node The  to compare for document order.
	 * @return if this node appears before the specified node; otherwise .
	 */
	function IsBefore(node:cs.system.xml.linq.XNode):Bool;
	/**
	 * Returns a collection of the sibling nodes after this node, in document order.
	 * @return An  of  of the sibling nodes after this node, in document order.
	 */
	function NodesAfterSelf():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	/**
	 * Returns a collection of the sibling nodes before this node, in document order.
	 * @return An  of  of the sibling nodes before this node, in document order.
	 */
	function NodesBeforeSelf():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	/** Removes this node from its parent. */
	function Remove():Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Replaces this node with the specified content.
	 * @param content Content that replaces this node.
	 */
	function ReplaceWith(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function():String {})
	/**
	 * Returns the indented XML for this node.
	 * @return A  containing the indented XML.
	 */
	function ToString(options:cs.system.xml.linq.SaveOptions):String;
	/**
	 * Writes this node to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}

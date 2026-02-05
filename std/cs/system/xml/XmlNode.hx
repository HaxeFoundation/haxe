package cs.system.xml;

/** Represents a single node in the XML document. */
@:native("System.Xml.XmlNode")
extern class XmlNode {
	/**
	 * Gets an  containing the attributes of this node.
	 * @return An  containing the attributes of the node. If the node is of type
	 * XmlNodeType.Element, the attributes of the node are returned. Otherwise, this
	 * property returns .
	 */
	var Attributes(default, never):cs.system.xml.XmlAttributeCollection;
	/**
	 * Gets the base URI of the current node.
	 * @return The location from which the node was loaded or String.Empty if the node
	 * has no base URI.
	 */
	var BaseURI(default, never):String;
	/**
	 * Gets all the child nodes of the node.
	 * @return An object that contains all the child nodes of the node. If there are no
	 * child nodes, this property returns an empty .
	 */
	var ChildNodes(default, never):cs.system.xml.XmlNodeList;
	/**
	 * Gets the first child of the node.
	 * @return The first child of the node. If there is no such node,  is returned.
	 */
	var FirstChild(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets a value indicating whether this node has any child nodes.
	 * @return if the node has child nodes; otherwise, .
	 */
	var HasChildNodes(default, never):Bool;
	/**
	 * Gets or sets the concatenated values of the node and all its child nodes.
	 * @return The concatenated values of the node and all its child nodes.
	 */
	var InnerText(default, default):String;
	/**
	 * Gets or sets the markup representing only the child nodes of this node.
	 * @return The markup of the child nodes of this node. does not return default
	 * attributes.
	 */
	var InnerXml(default, default):String;
	/**
	 * Gets a value indicating whether the node is read-only.
	 * @return if the node is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets the last child of the node.
	 * @return The last child of the node. If there is no such node,  is returned.
	 */
	var LastChild(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the local name of the node, when overridden in a derived class.
	 * @return The name of the node with the prefix removed. For example,  is book for
	 * the element <bk:book>. The name returned is dependent on the  of the node: Type
	 * Name Attribute The local name of the attribute. CDATA #cdata-section Comment
	 * #comment Document #document DocumentFragment #document-fragment DocumentType The
	 * document type name. Element The local name of the element. Entity The name of
	 * the entity. EntityReference The name of the entity referenced. Notation The
	 * notation name. ProcessingInstruction The target of the processing instruction.
	 * Text #text Whitespace #whitespace SignificantWhitespace #significant-whitespace
	 * XmlDeclaration #xml-declaration
	 */
	var LocalName(default, never):String;
	/**
	 * Gets the qualified name of the node, when overridden in a derived class.
	 * @return The qualified name of the node. The name returned is dependent on the 
	 * of the node: Type Name Attribute The qualified name of the attribute. CDATA
	 * #cdata-section Comment #comment Document #document DocumentFragment
	 * #document-fragment DocumentType The document type name. Element The qualified
	 * name of the element. Entity The name of the entity. EntityReference The name of
	 * the entity referenced. Notation The notation name. ProcessingInstruction The
	 * target of the processing instruction. Text #text Whitespace #whitespace
	 * SignificantWhitespace #significant-whitespace XmlDeclaration #xml-declaration
	 */
	var Name(default, never):String;
	/**
	 * Gets the namespace URI of this node.
	 * @return The namespace URI of this node. If there is no namespace URI, this
	 * property returns String.Empty.
	 */
	var NamespaceURI(default, never):String;
	/**
	 * Gets the node immediately following this node.
	 * @return The next . If there is no next node,  is returned.
	 */
	var NextSibling(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the type of the current node, when overridden in a derived class.
	 * @return One of the  values.
	 */
	var NodeType(default, never):cs.system.xml.XmlNodeType;
	/**
	 * Gets the markup containing this node and all its child nodes.
	 * @return The markup containing this node and all its child nodes. does not return
	 * default attributes.
	 */
	var OuterXml(default, never):String;
	/**
	 * Gets the  to which this node belongs.
	 * @return The  to which this node belongs. If the node is an  (NodeType equals
	 * XmlNodeType.Document), this property returns .
	 */
	var OwnerDocument(default, never):cs.system.xml.XmlDocument;
	/**
	 * Gets the parent of this node (for nodes that can have parents).
	 * @return The  that is the parent of the current node. If a node has just been
	 * created and not yet added to the tree, or if it has been removed from the tree,
	 * the parent is . For all other nodes, the value returned depends on the  of the
	 * node. The following table describes the possible return values for the 
	 * property. NodeType Return Value of ParentNode Attribute, Document,
	 * DocumentFragment, Entity, Notation Returns ; these nodes do not have parents.
	 * CDATA Returns the element or entity reference containing the CDATA section.
	 * Comment Returns the element, entity reference, document type, or document
	 * containing the comment. DocumentType Returns the document node. Element Returns
	 * the parent node of the element. If the element is the root node in the tree, the
	 * parent is the document node. EntityReference Returns the element, attribute, or
	 * entity reference containing the entity reference. ProcessingInstruction Returns
	 * the document, element, document type, or entity reference containing the
	 * processing instruction. Text Returns the parent element, attribute, or entity
	 * reference containing the text node.
	 */
	var ParentNode(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets or sets the namespace prefix of this node.
	 * @return The namespace prefix of this node. For example,  is bk for the element
	 * <bk:book>. If there is no prefix, this property returns String.Empty.
	 */
	var Prefix(default, default):String;
	/**
	 * Gets the node immediately preceding this node.
	 * @return The preceding . If there is no preceding node,  is returned.
	 */
	var PreviousSibling(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the text node that immediately precedes this node.
	 * @return Returns .
	 */
	var PreviousText(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the post schema validation infoset that has been assigned to this node as a
	 * result of schema validation.
	 * @return An  object containing the post schema validation infoset of this node.
	 */
	var SchemaInfo(default, never):cs.system.xml.schema.IXmlSchemaInfo;
	/**
	 * Gets or sets the value of the node.
	 * @return The value returned depends on the  of the node: Type Value Attribute The
	 * value of the attribute. CDATASection The content of the CDATA Section. Comment
	 * The content of the comment. Document . DocumentFragment . DocumentType . Element
	 * . You can use the  or  properties to access the value of the element node.
	 * Entity . EntityReference . Notation . ProcessingInstruction The entire content
	 * excluding the target. Text The content of the text node. SignificantWhitespace
	 * The white space characters. White space can consist of one or more space
	 * characters, carriage returns, line feeds, or tabs. Whitespace The white space
	 * characters. White space can consist of one or more space characters, carriage
	 * returns, line feeds, or tabs. XmlDeclaration The content of the declaration
	 * (that is, everything between <?xml and ?>).
	 */
	var Value(default, default):String;
	@:overload(function(index0:String):cs.system.xml.XmlElement {})
	@:native("get_Item")
	function get_Item(index0:String, index1:String):cs.system.xml.XmlElement;
	/**
	 * Adds the specified node to the end of the list of child nodes, of this node.
	 * @param newChild The node to add. All the contents of the node to be added are
	 * moved into the specified location.
	 * @return The node added.
	 */
	function AppendChild(newChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Creates a duplicate of this node.
	 * @return The cloned node.
	 */
	function Clone():cs.system.xml.XmlNode;
	/**
	 * Creates a duplicate of the node, when overridden in a derived class.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Creates an  for navigating this object.
	 * @return An  object used to navigate the node. The  is positioned on the node
	 * from which the method was called. It is not positioned on the root of the
	 * document.
	 */
	function CreateNavigator():cs.system.xml.xpath.XPathNavigator;
	/**
	 * Gets an enumerator that iterates through the child nodes in the current node.
	 * @return An  object that can be used to iterate through the child nodes in the
	 * current node.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Looks up the closest xmlns declaration for the given prefix that is in scope for
	 * the current node and returns the namespace URI in the declaration.
	 * @param prefix The prefix whose namespace URI you want to find.
	 * @return The namespace URI of the specified prefix.
	 */
	function GetNamespaceOfPrefix(prefix:String):String;
	/**
	 * Looks up the closest xmlns declaration for the given namespace URI that is in
	 * scope for the current node and returns the prefix defined in that declaration.
	 * @param namespaceURI The namespace URI whose prefix you want to find.
	 * @return The prefix for the specified namespace URI.
	 */
	function GetPrefixOfNamespace(namespaceURI:String):String;
	/**
	 * Inserts the specified node immediately after the specified reference node.
	 * @param newChild The node to insert.
	 * @param refChild The reference node.  is placed after .
	 * @return The node being inserted.
	 */
	function InsertAfter(newChild:cs.system.xml.XmlNode, refChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Inserts the specified node immediately before the specified reference node.
	 * @param newChild The node to insert.
	 * @param refChild The reference node.  is placed before this node.
	 * @return The node being inserted.
	 */
	function InsertBefore(newChild:cs.system.xml.XmlNode, refChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/** Puts all XmlText nodes in the full depth of the sub-tree underneath this XmlNode into a "normal" form where only markup (that is, tags, comments, processing instructions, CDATA sections, and entity references) separates XmlText nodes, that is, there are no adjacent XmlText nodes. */
	function Normalize():Void;
	/**
	 * Adds the specified node to the beginning of the list of child nodes for this
	 * node.
	 * @param newChild The node to add. All the contents of the node to be added are
	 * moved into the specified location.
	 * @return The node added.
	 */
	function PrependChild(newChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/** Removes all the child nodes and/or attributes of the current node. */
	function RemoveAll():Void;
	/**
	 * Removes specified child node.
	 * @param oldChild The node being removed.
	 * @return The node removed.
	 */
	function RemoveChild(oldChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Replaces the child node  with  node.
	 * @param newChild The new node to put in the child list.
	 * @param oldChild The node being replaced in the list.
	 * @return The node replaced.
	 */
	function ReplaceChild(newChild:cs.system.xml.XmlNode, oldChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	@:overload(function(xpath:String):cs.system.xml.XmlNodeList {})
	/**
	 * Selects a list of nodes matching the XPath expression.
	 * @param xpath The XPath expression.
	 * @return An  containing a collection of nodes matching the XPath query.
	 */
	function SelectNodes(xpath:String, nsmgr:cs.system.xml.XmlNamespaceManager):cs.system.xml.XmlNodeList;
	@:overload(function(xpath:String):cs.system.xml.XmlNode {})
	/**
	 * Selects the first  that matches the XPath expression.
	 * @param xpath The XPath expression. See XPath Examples.
	 * @return The first  that matches the XPath query or  if no matching node is
	 * found.
	 */
	function SelectSingleNode(xpath:String, nsmgr:cs.system.xml.XmlNamespaceManager):cs.system.xml.XmlNode;
	/**
	 * Tests if the DOM implementation implements a specific feature.
	 * @param feature The package name of the feature to test. This name is not
	 * case-sensitive.
	 * @param version The version number of the package name to test. If the version is
	 * not specified (null), supporting any version of the feature causes the method to
	 * return true.
	 * @return if the feature is implemented in the specified version; otherwise, . The
	 * following table describes the combinations that return . Feature Version XML 1.0
	 * XML 2.0
	 */
	function Supports(feature:String, version:String):Bool;
	/**
	 * Saves all the child nodes of the node to the specified , when overridden in a
	 * derived class.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the current node to the specified , when overridden in a derived class.
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}

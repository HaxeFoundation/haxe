package cs.system.xml.xpath;

/** Provides a cursor model for navigating and editing XML data. */
@:native("System.Xml.XPath.XPathNavigator")
extern class XPathNavigator extends cs.system.xml.xpath.XPathItem {
	/**
	 * Gets an  used for equality comparison of  objects.
	 * @return An  used for equality comparison of  objects.
	 */
	static var NavigatorComparer(default, never):cs.system.collections.IEqualityComparer;
	/**
	 * When overridden in a derived class, gets the base URI for the current node.
	 * @return The location from which the node was loaded, or  if there is no value.
	 */
	var BaseURI(default, never):String;
	/**
	 * Gets a value that indicates whether the  can edit the underlying XML data.
	 * @return if the  can edit the underlying XML data; otherwise, .
	 */
	var CanEdit(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current node has any attributes.
	 * @return if the current node has attributes; returns  if the current node has no
	 * attributes, or if the  is not positioned on an element node.
	 */
	var HasAttributes(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current node has any child nodes.
	 * @return if the current node has any child nodes; otherwise, .
	 */
	var HasChildren(default, never):Bool;
	/**
	 * Gets or sets the markup representing the child nodes of the current node.
	 * @return A  that contains the markup of the child nodes of the current node.
	 */
	var InnerXml(default, default):String;
	/**
	 * When overridden in a derived class, gets a value that indicates whether the
	 * current node is an empty element without an end element tag.
	 * @return if the current node is an empty element; otherwise, .
	 */
	var IsEmptyElement(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the  of the current node without any
	 * namespace prefix.
	 * @return A  that contains the local name of the current node, or  if the current
	 * node does not have a name (for example, text or comment nodes).
	 */
	var LocalName(default, never):String;
	/**
	 * When overridden in a derived class, gets the qualified name of the current node.
	 * @return A  that contains the qualified  of the current node, or  if the current
	 * node does not have a name (for example, text or comment nodes).
	 */
	var Name(default, never):String;
	/**
	 * When overridden in a derived class, gets the namespace URI of the current node.
	 * @return A  that contains the namespace URI of the current node, or  if the
	 * current node has no namespace URI.
	 */
	var NamespaceURI(default, never):String;
	/**
	 * When overridden in a derived class, gets the  of the .
	 * @return An  object enabling you to get the atomized version of a  within the XML
	 * document.
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	/**
	 * When overridden in a derived class, gets the  of the current node.
	 * @return One of the  values representing the current node.
	 */
	var NodeType(default, never):cs.system.xml.xpath.XPathNodeType;
	/**
	 * Gets or sets the markup representing the opening and closing tags of the current
	 * node and its child nodes.
	 * @return A  that contains the markup representing the opening and closing tags of
	 * the current node and its child nodes.
	 */
	var OuterXml(default, default):String;
	/**
	 * When overridden in a derived class, gets the namespace prefix associated with
	 * the current node.
	 * @return A  that contains the namespace prefix associated with the current node.
	 */
	var Prefix(default, never):String;
	/**
	 * Gets the schema information that has been assigned to the current node as a
	 * result of schema validation.
	 * @return An  object that contains the schema information for the current node.
	 */
	var SchemaInfo(default, never):cs.system.xml.schema.IXmlSchemaInfo;
	/**
	 * Used by  implementations which provide a "virtualized" XML view over a store, to
	 * provide access to underlying objects.
	 * @return The default is .
	 */
	var UnderlyingObject(default, never):Dynamic;
	/**
	 * Gets the xml:lang scope for the current node.
	 * @return A  that contains the value of the xml:lang scope, or  if the current
	 * node has no xml:lang scope value to return.
	 */
	var XmlLang(default, never):String;
	@:overload(function():cs.system.xml.XmlWriter {})
	@:overload(function(newChild:String):Void {})
	@:overload(function(newChild:cs.system.xml.XmlReader):Void {})
	/**
	 * Returns an  object used to create one or more new child nodes at the end of the
	 * list of child nodes of the current node.
	 * @return An  object used to create new child nodes at the end of the list of
	 * child nodes of the current node.
	 */
	function AppendChild(newChild:cs.system.xml.xpath.XPathNavigator):Void;
	/**
	 * Creates a new child element node at the end of the list of child nodes of the
	 * current node using the namespace prefix, local name and namespace URI specified
	 * with the value specified.
	 * @param prefix The namespace prefix of the new child element node (if any).
	 * @param localName The local name of the new child element node (if any).
	 * @param namespaceURI The namespace URI of the new child element node (if any). 
	 * and  are equivalent.
	 * @param value The value of the new child element node. If  or  are passed, an
	 * empty element is created.
	 */
	function AppendChildElement(prefix:String, localName:String, namespaceURI:String, value:String):Void;
	/**
	 * Verifies that the XML data in the  conforms to the XML Schema definition
	 * language (XSD) schema provided.
	 * @param schemas The  containing the schemas used to validate the XML data
	 * contained in the .
	 * @param validationEventHandler The  that receives information about schema
	 * validation warnings and errors.
	 * @return if no schema validation errors occurred; otherwise, .
	 */
	function CheckValidity(schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Bool;
	/**
	 * When overridden in a derived class, creates a new  positioned at the same node
	 * as this .
	 * @return A new  positioned at the same node as this .
	 */
	function Clone():cs.system.xml.xpath.XPathNavigator;
	/**
	 * Compares the position of the current  with the position of the  specified.
	 * @param nav The  to compare against.
	 * @return An  value representing the comparative position of the two  objects.
	 */
	function ComparePosition(nav:cs.system.xml.xpath.XPathNavigator):cs.system.xml.XmlNodeOrder;
	/**
	 * Compiles a string representing an XPath expression and returns an  object.
	 * @param xpath A string representing an XPath expression.
	 * @return An  object representing the XPath expression.
	 */
	function Compile(xpath:String):cs.system.xml.xpath.XPathExpression;
	/**
	 * Creates an attribute node on the current element node using the namespace
	 * prefix, local name and namespace URI specified with the value specified.
	 * @param prefix The namespace prefix of the new attribute node (if any).
	 * @param localName The local name of the new attribute node which cannot  or .
	 * @param namespaceURI The namespace URI for the new attribute node (if any).
	 * @param value The value of the new attribute node. If  or  are passed, an empty
	 * attribute node is created.
	 */
	function CreateAttribute(prefix:String, localName:String, namespaceURI:String, value:String):Void;
	/**
	 * Returns an  object used to create new attributes on the current element.
	 * @return An  object used to create new attributes on the current element.
	 */
	function CreateAttributes():cs.system.xml.XmlWriter;
	/**
	 * Returns a copy of the .
	 * @return An  copy of this .
	 */
	function CreateNavigator():cs.system.xml.xpath.XPathNavigator;
	/**
	 * Deletes a range of sibling nodes from the current node to the node specified.
	 * @param lastSiblingToDelete An  positioned on the last sibling node in the range
	 * to delete.
	 */
	function DeleteRange(lastSiblingToDelete:cs.system.xml.xpath.XPathNavigator):Void;
	/** Deletes the current node and its child nodes. */
	function DeleteSelf():Void;
	@:overload(function(xpath:String):Dynamic {})
	@:overload(function(expr:cs.system.xml.xpath.XPathExpression):Dynamic {})
	@:overload(function(xpath:String, resolver:cs.system.xml.IXmlNamespaceResolver):Dynamic {})
	/**
	 * Evaluates the specified XPath expression and returns the typed result.
	 * @param xpath A string representing an XPath expression that can be evaluated.
	 * @return The result of the expression (Boolean, number, string, or node set).
	 * This maps to , , , or  objects respectively.
	 */
	function Evaluate(expr:cs.system.xml.xpath.XPathExpression, context:cs.system.xml.xpath.XPathNodeIterator):Dynamic;
	/**
	 * Gets the value of the attribute with the specified local name and namespace URI.
	 * @param localName The local name of the attribute.  is case-sensitive.
	 * @param namespaceURI The namespace URI of the attribute.
	 * @return A  that contains the value of the specified attribute;  if a matching
	 * attribute is not found, or if the  is not positioned on an element node.
	 */
	function GetAttribute(localName:String, namespaceURI:String):String;
	/**
	 * Returns the value of the namespace node corresponding to the specified local
	 * name.
	 * @param name The local name of the namespace node.
	 * @return A  that contains the value of the namespace node;  if a matching
	 * namespace node is not found, or if the  is not positioned on an element node.
	 */
	function GetNamespace(name:String):String;
	/**
	 * Returns the in-scope namespaces of the current node.
	 * @param scope An  value specifying the namespaces to return.
	 * @return An  collection of namespace names keyed by prefix.
	 */
	function GetNamespacesInScope(scope:cs.system.xml.XmlNamespaceScope):cs.system.collections.generic.IDictionary<String, String>;
	@:overload(function():cs.system.xml.XmlWriter {})
	@:overload(function(newSibling:String):Void {})
	@:overload(function(newSibling:cs.system.xml.XmlReader):Void {})
	/**
	 * Returns an  object used to create a new sibling node after the currently
	 * selected node.
	 * @return An  object used to create a new sibling node after the currently
	 * selected node.
	 */
	function InsertAfter(newSibling:cs.system.xml.xpath.XPathNavigator):Void;
	@:overload(function():cs.system.xml.XmlWriter {})
	@:overload(function(newSibling:String):Void {})
	@:overload(function(newSibling:cs.system.xml.XmlReader):Void {})
	/**
	 * Returns an  object used to create a new sibling node before the currently
	 * selected node.
	 * @return An  object used to create a new sibling node before the currently
	 * selected node.
	 */
	function InsertBefore(newSibling:cs.system.xml.xpath.XPathNavigator):Void;
	/**
	 * Creates a new sibling element after the current node using the namespace prefix,
	 * local name and namespace URI specified, with the value specified.
	 * @param prefix The namespace prefix of the new child element (if any).
	 * @param localName The local name of the new child element (if any).
	 * @param namespaceURI The namespace URI of the new child element (if any).  and 
	 * are equivalent.
	 * @param value The value of the new child element. If  or  are passed, an empty
	 * element is created.
	 */
	function InsertElementAfter(prefix:String, localName:String, namespaceURI:String, value:String):Void;
	/**
	 * Creates a new sibling element before the current node using the namespace
	 * prefix, local name, and namespace URI specified, with the value specified.
	 * @param prefix The namespace prefix of the new child element (if any).
	 * @param localName The local name of the new child element (if any).
	 * @param namespaceURI The namespace URI of the new child element (if any).  and 
	 * are equivalent.
	 * @param value The value of the new child element. If  or  are passed, an empty
	 * element is created.
	 */
	function InsertElementBefore(prefix:String, localName:String, namespaceURI:String, value:String):Void;
	/**
	 * Determines whether the specified  is a descendant of the current .
	 * @param nav The  to compare to this .
	 * @return if the specified  is a descendant of the current ; otherwise, .
	 */
	function IsDescendant(nav:cs.system.xml.xpath.XPathNavigator):Bool;
	/**
	 * When overridden in a derived class, determines whether the current  is at the
	 * same position as the specified .
	 * @param other The  to compare to this .
	 * @return if the two  objects have the same position; otherwise, .
	 */
	function IsSamePosition(other:cs.system.xml.xpath.XPathNavigator):Bool;
	/**
	 * Gets the namespace URI for the specified prefix.
	 * @param prefix The prefix whose namespace URI you want to resolve. To match the
	 * default namespace, pass .
	 * @return A  that contains the namespace URI assigned to the namespace prefix
	 * specified;  if no namespace URI is assigned to the prefix specified. The 
	 * returned is atomized.
	 */
	function LookupNamespace(prefix:String):String;
	/**
	 * Gets the prefix declared for the specified namespace URI.
	 * @param namespaceURI The namespace URI to resolve for the prefix.
	 * @return A  that contains the namespace prefix assigned to the namespace URI
	 * specified; otherwise,  if no prefix is assigned to the namespace URI specified.
	 * The  returned is atomized.
	 */
	function LookupPrefix(namespaceURI:String):String;
	@:overload(function(xpath:String):Bool {})
	/**
	 * Determines whether the current node matches the specified XPath expression.
	 * @param xpath The XPath expression.
	 * @return if the current node matches the specified XPath expression; otherwise, .
	 */
	function Matches(expr:cs.system.xml.xpath.XPathExpression):Bool;
	/**
	 * When overridden in a derived class, moves the  to the same position as the
	 * specified .
	 * @param other The  positioned on the node that you want to move to.
	 * @return if the  is successful moving to the same position as the specified ;
	 * otherwise, . If , the position of the  is unchanged.
	 */
	function MoveTo(other:cs.system.xml.xpath.XPathNavigator):Bool;
	/**
	 * Moves the  to the attribute with the matching local name and namespace URI.
	 * @param localName The local name of the attribute.
	 * @param namespaceURI The namespace URI of the attribute;  for an empty namespace.
	 * @return if the  is successful moving to the attribute; otherwise, . If , the
	 * position of the  is unchanged.
	 */
	function MoveToAttribute(localName:String, namespaceURI:String):Bool;
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType):Bool {})
	/**
	 * Moves the  to the child node with the local name and namespace URI specified.
	 * @param localName The local name of the child node to move to.
	 * @param namespaceURI The namespace URI of the child node to move to.
	 * @return if the  is successful moving to the child node; otherwise, . If , the
	 * position of the  is unchanged.
	 */
	function MoveToChild(localName:String, namespaceURI:String):Bool;
	/**
	 * Moves the  to the first sibling node of the current node.
	 * @return if the  is successful moving to the first sibling node of the current
	 * node;  if there is no first sibling, or if the  is currently positioned on an
	 * attribute node. If the  is already positioned on the first sibling,  will return
	 * and will not move its position. If  returns  because there is no first sibling,
	 * or if  is currently positioned on an attribute, the position of the  is
	 * unchanged.
	 */
	function MoveToFirst():Bool;
	/**
	 * When overridden in a derived class, moves the  to the first attribute of the
	 * current node.
	 * @return if the  is successful moving to the first attribute of the current node;
	 * otherwise, . If , the position of the  is unchanged.
	 */
	function MoveToFirstAttribute():Bool;
	/**
	 * When overridden in a derived class, moves the  to the first child node of the
	 * current node.
	 * @return if the  is successful moving to the first child node of the current
	 * node; otherwise, . If , the position of the  is unchanged.
	 */
	function MoveToFirstChild():Bool;
	@:overload(function():Bool {})
	/**
	 * Moves the  to first namespace node of the current node.
	 * @return if the  is successful moving to the first namespace node; otherwise, .
	 * If , the position of the  is unchanged.
	 */
	function MoveToFirstNamespace(namespaceScope:cs.system.xml.xpath.XPathNamespaceScope):Bool;
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType):Bool {})
	@:overload(function(localName:String, namespaceURI:String):Bool {})
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType, end:cs.system.xml.xpath.XPathNavigator):Bool {})
	/**
	 * Moves the  to the element with the local name and namespace URI specified in
	 * document order.
	 * @param localName The local name of the element.
	 * @param namespaceURI The namespace URI of the element.
	 * @return if the  moved successfully; otherwise, .
	 */
	function MoveToFollowing(localName:String, namespaceURI:String, end:cs.system.xml.xpath.XPathNavigator):Bool;
	/**
	 * When overridden in a derived class, moves to the node that has an attribute of
	 * type ID whose value matches the specified .
	 * @param id A  representing the ID value of the node to which you want to move.
	 * @return if the  is successful moving; otherwise, . If , the position of the
	 * navigator is unchanged.
	 */
	function MoveToId(id:String):Bool;
	/**
	 * Moves the  to the namespace node with the specified namespace prefix.
	 * @param name The namespace prefix of the namespace node.
	 * @return if the  is successful moving to the specified namespace;  if a matching
	 * namespace node was not found, or if the  is not positioned on an element node.
	 * If , the position of the  is unchanged.
	 */
	function MoveToNamespace(name:String):Bool;
	@:overload(function():Bool {})
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType):Bool {})
	/**
	 * When overridden in a derived class, moves the  to the next sibling node of the
	 * current node.
	 * @return if the  is successful moving to the next sibling node; otherwise,  if
	 * there are no more siblings or if the  is currently positioned on an attribute
	 * node. If , the position of the  is unchanged.
	 */
	function MoveToNext(localName:String, namespaceURI:String):Bool;
	/**
	 * When overridden in a derived class, moves the  to the next attribute.
	 * @return if the  is successful moving to the next attribute;  if there are no
	 * more attributes. If , the position of the  is unchanged.
	 */
	function MoveToNextAttribute():Bool;
	@:overload(function():Bool {})
	/**
	 * Moves the  to the next namespace node.
	 * @return if the  is successful moving to the next namespace node; otherwise, . If
	 * , the position of the  is unchanged.
	 */
	function MoveToNextNamespace(namespaceScope:cs.system.xml.xpath.XPathNamespaceScope):Bool;
	/**
	 * When overridden in a derived class, moves the  to the parent node of the current
	 * node.
	 * @return if the  is successful moving to the parent node of the current node;
	 * otherwise, . If , the position of the  is unchanged.
	 */
	function MoveToParent():Bool;
	/**
	 * When overridden in a derived class, moves the  to the previous sibling node of
	 * the current node.
	 * @return if the  is successful moving to the previous sibling node; otherwise, 
	 * if there is no previous sibling node or if the  is currently positioned on an
	 * attribute node. If , the position of the  is unchanged.
	 */
	function MoveToPrevious():Bool;
	/** Moves the  to the root node that the current node belongs to. */
	function MoveToRoot():Void;
	@:overload(function():cs.system.xml.XmlWriter {})
	@:overload(function(newChild:String):Void {})
	@:overload(function(newChild:cs.system.xml.XmlReader):Void {})
	/**
	 * Returns an  object used to create a new child node at the beginning of the list
	 * of child nodes of the current node.
	 * @return An  object used to create a new child node at the beginning of the list
	 * of child nodes of the current node.
	 */
	function PrependChild(newChild:cs.system.xml.xpath.XPathNavigator):Void;
	/**
	 * Creates a new child element at the beginning of the list of child nodes of the
	 * current node using the namespace prefix, local name, and namespace URI specified
	 * with the value specified.
	 * @param prefix The namespace prefix of the new child element (if any).
	 * @param localName The local name of the new child element (if any).
	 * @param namespaceURI The namespace URI of the new child element (if any).  and 
	 * are equivalent.
	 * @param value The value of the new child element. If  or  are passed, an empty
	 * element is created.
	 */
	function PrependChildElement(prefix:String, localName:String, namespaceURI:String, value:String):Void;
	/**
	 * Returns an  object that contains the current node and its child nodes.
	 * @return An  object that contains the current node and its child nodes.
	 */
	function ReadSubtree():cs.system.xml.XmlReader;
	/**
	 * Replaces a range of sibling nodes from the current node to the node specified.
	 * @param lastSiblingToReplace An  positioned on the last sibling node in the range
	 * to replace.
	 * @return An  object used to specify the replacement range.
	 */
	function ReplaceRange(lastSiblingToReplace:cs.system.xml.xpath.XPathNavigator):cs.system.xml.XmlWriter;
	@:overload(function(newNode:String):Void {})
	@:overload(function(newNode:cs.system.xml.XmlReader):Void {})
	/**
	 * Replaces the current node with the content of the string specified.
	 * @param newNode The XML data string for the new node.
	 */
	function ReplaceSelf(newNode:cs.system.xml.xpath.XPathNavigator):Void;
	@:overload(function(xpath:String):cs.system.xml.xpath.XPathNodeIterator {})
	@:overload(function(expr:cs.system.xml.xpath.XPathExpression):cs.system.xml.xpath.XPathNodeIterator {})
	/**
	 * Selects a node set, using the specified XPath expression.
	 * @param xpath A  representing an XPath expression.
	 * @return An  pointing to the selected node set.
	 */
	function Select(xpath:String, resolver:cs.system.xml.IXmlNamespaceResolver):cs.system.xml.xpath.XPathNodeIterator;
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType, matchSelf:Bool):cs.system.xml.xpath.XPathNodeIterator {})
	/**
	 * Selects all the ancestor nodes of the current node that have the specified local
	 * name and namespace URI.
	 * @param name The local name of the ancestor nodes.
	 * @param namespaceURI The namespace URI of the ancestor nodes.
	 * @param matchSelf To include the context node in the selection, ; otherwise, .
	 * @return An  that contains the selected nodes. The returned nodes are in reverse
	 * document order.
	 */
	function SelectAncestors(name:String, namespaceURI:String, matchSelf:Bool):cs.system.xml.xpath.XPathNodeIterator;
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType):cs.system.xml.xpath.XPathNodeIterator {})
	/**
	 * Selects all the child nodes of the current node that have the local name and
	 * namespace URI specified.
	 * @param name The local name of the child nodes.
	 * @param namespaceURI The namespace URI of the child nodes.
	 * @return An  that contains the selected nodes.
	 */
	function SelectChildren(name:String, namespaceURI:String):cs.system.xml.xpath.XPathNodeIterator;
	@:overload(function(type:cs.system.xml.xpath.XPathNodeType, matchSelf:Bool):cs.system.xml.xpath.XPathNodeIterator {})
	/**
	 * Selects all the descendant nodes of the current node with the local name and
	 * namespace URI specified.
	 * @param name The local name of the descendant nodes.
	 * @param namespaceURI The namespace URI of the descendant nodes.
	 * @param matchSelf to include the context node in the selection; otherwise, .
	 * @return An  that contains the selected nodes.
	 */
	function SelectDescendants(name:String, namespaceURI:String, matchSelf:Bool):cs.system.xml.xpath.XPathNodeIterator;
	@:overload(function(xpath:String):cs.system.xml.xpath.XPathNavigator {})
	@:overload(function(expression:cs.system.xml.xpath.XPathExpression):cs.system.xml.xpath.XPathNavigator {})
	/**
	 * Selects a single node in the  using the specified XPath query.
	 * @param xpath A  representing an XPath expression.
	 * @return An  object that contains the first matching node for the XPath query
	 * specified; otherwise,  if there are no query results.
	 */
	function SelectSingleNode(xpath:String, resolver:cs.system.xml.IXmlNamespaceResolver):cs.system.xml.xpath.XPathNavigator;
	/**
	 * Sets the typed value of the current node.
	 * @param typedValue The new typed value of the node.
	 */
	function SetTypedValue(typedValue:Dynamic):Void;
	/**
	 * Sets the value of the current node.
	 * @param value The new value of the node.
	 */
	function SetValue(value:String):Void;
	/**
	 * Gets the text value of the current node.
	 * @return A  that contains the text value of the current node.
	 */
	function ToString():String;
	/**
	 * Gets the current node's value as the  specified, using the  object specified to
	 * resolve namespace prefixes.
	 * @param returnType The  to return the current node's value as.
	 * @param nsResolver The  object used to resolve namespace prefixes.
	 * @return The value of the current node as the  requested.
	 */
	function ValueAs(returnType:cs.system.Type, nsResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
	/**
	 * Streams the current node and its child nodes to the  object specified.
	 * @param writer The  object to stream to.
	 */
	function WriteSubtree(writer:cs.system.xml.XmlWriter):Void;
}

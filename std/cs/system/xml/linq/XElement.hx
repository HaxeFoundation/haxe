package cs.system.xml.linq;

/** Represents an XML element.  See XElement Class Overview and the Remarks section on this page for usage information and examples. */
@:native("System.Xml.Linq.XElement")
extern class XElement extends cs.system.xml.linq.XContainer {
	/**
	 * Gets an empty collection of elements.
	 * @return An  of  that contains an empty collection.
	 */
	static var EmptySequence(default, never):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Gets the first attribute of this element.
	 * @return An  that contains the first attribute of this element.
	 */
	var FirstAttribute(default, never):cs.system.xml.linq.XAttribute;
	/**
	 * Gets a value indicating whether this element has at least one attribute.
	 * @return if this element has at least one attribute; otherwise .
	 */
	var HasAttributes(default, never):Bool;
	/**
	 * Gets a value indicating whether this element has at least one child element.
	 * @return if this element has at least one child element; otherwise .
	 */
	var HasElements(default, never):Bool;
	/**
	 * Gets a value indicating whether this element contains no content.
	 * @return if this element contains no content; otherwise .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets the last attribute of this element.
	 * @return An  that contains the last attribute of this element.
	 */
	var LastAttribute(default, never):cs.system.xml.linq.XAttribute;
	/**
	 * Gets or sets the name of this element.
	 * @return An  that contains the name of this element.
	 */
	var Name(default, default):cs.system.xml.linq.XName;
	/**
	 * Gets or sets the concatenated text contents of this element.
	 * @return A  that contains all of the text content of this element. If there are
	 * multiple text nodes, they will be concatenated.
	 */
	var Value(default, default):String;
	@:overload(function(other:cs.system.xml.linq.XElement):Void {})
	@:overload(function(name:cs.system.xml.linq.XName):Void {})
	@:overload(function(other:cs.system.xml.linq.XStreamingElement):Void {})
	@:overload(function(name:cs.system.xml.linq.XName, content:Dynamic):Void {})
	function new(name:cs.system.xml.linq.XName, content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(stream:cs.system.io.Stream):cs.system.xml.linq.XElement {})
	@:overload(function(textReader:cs.system.io.TextReader):cs.system.xml.linq.XElement {})
	@:overload(function(uri:String):cs.system.xml.linq.XElement {})
	@:overload(function(reader:cs.system.xml.XmlReader):cs.system.xml.linq.XElement {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XElement {})
	@:overload(function(textReader:cs.system.io.TextReader, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XElement {})
	@:overload(function(uri:String, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XElement {})
	/**
	 * Creates a new  instance by using the specified stream.
	 * @param stream The stream that contains the XML data.
	 * @return An  object used to read the data that is contained in the stream.
	 */
	static function Load(reader:cs.system.xml.XmlReader, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XElement;
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XElement> {})
	@:overload(function(textReader:cs.system.io.TextReader, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XElement> {})
	/**
	 * @param stream 
	 * @param options 
	 * @param cancellationToken 
	 */
	static function LoadAsync(reader:cs.system.xml.XmlReader, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XElement>;
	@:overload(function(element:cs.system.xml.linq.XElement):Bool {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.system.DateTime {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.system.DateTimeOffset {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.system.Decimal {})
	@:overload(function(element:cs.system.xml.linq.XElement):Float {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.system.Guid {})
	@:overload(function(element:cs.system.xml.linq.XElement):Int {})
	@:overload(function(element:cs.system.xml.linq.XElement):haxe.Int64 {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<Bool> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.system.DateTimeOffset> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.system.DateTime> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.system.Decimal> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<Float> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.system.Guid> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<Int> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<haxe.Int64> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<Single> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.system.TimeSpan> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.UInt> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Null<cs.UInt64> {})
	@:overload(function(element:cs.system.xml.linq.XElement):Single {})
	@:overload(function(element:cs.system.xml.linq.XElement):String {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.system.TimeSpan {})
	@:overload(function(element:cs.system.xml.linq.XElement):cs.UInt {})
	/**
	 * Cast the value of this  to a .
	 * @param element The  to cast to .
	 * @return A  that contains the content of this .
	 */
	static function op_Explicit(element:cs.system.xml.linq.XElement):cs.UInt64;
	@:overload(function(text:String):cs.system.xml.linq.XElement {})
	/**
	 * Load an  from a string that contains XML.
	 * @param text A  that contains XML.
	 * @return An  populated from the string that contains XML.
	 */
	static function Parse(text:String, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XElement;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contain this element, and the ancestors of
	 * this element.
	 * @return An  of  of elements that contain this element, and the ancestors of this
	 * element.
	 */
	function AncestorsAndSelf(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Returns the  of this  that has the specified .
	 * @param name The  of the  to get.
	 * @return An  that has the specified ;  if there is no attribute with the
	 * specified name.
	 */
	function Attribute(name:cs.system.xml.linq.XName):cs.system.xml.linq.XAttribute;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute> {})
	/**
	 * Returns a collection of attributes of this element.
	 * @return An  of  of attributes of this element.
	 */
	function Attributes(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute>;
	/**
	 * Returns a collection of nodes that contain this element, and all descendant
	 * nodes of this element, in document order.
	 * @return An  of  that contain this element, and all descendant nodes of this
	 * element, in document order.
	 */
	function DescendantNodesAndSelf():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contain this element, and all descendant
	 * elements of this element, in document order.
	 * @return An  of  of elements that contain this element, and all descendant
	 * elements of this element, in document order.
	 */
	function DescendantsAndSelf(name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Gets the default  of this .
	 * @return An  that contains the default namespace of this .
	 */
	function GetDefaultNamespace():cs.system.xml.linq.XNamespace;
	/**
	 * Gets the namespace associated with a particular prefix for this .
	 * @param prefix A string that contains the namespace prefix to look up.
	 * @return An  for the namespace associated with the prefix for this .
	 */
	function GetNamespaceOfPrefix(prefix:String):cs.system.xml.linq.XNamespace;
	/**
	 * Gets the prefix associated with a namespace for this .
	 * @param ns An  to look up.
	 * @return A  that contains the namespace prefix.
	 */
	function GetPrefixOfNamespace(ns:cs.system.xml.linq.XNamespace):String;
	/** Removes nodes and attributes from this . */
	function RemoveAll():Void;
	/** Removes the attributes of this . */
	function RemoveAttributes():Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Replaces the child nodes and the attributes of this element with the specified
	 * content.
	 * @param content The content that will replace the child nodes and attributes of
	 * this element.
	 */
	function ReplaceAll(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Replaces the attributes of this element with the specified content.
	 * @param content The content that will replace the attributes of this element.
	 */
	function ReplaceAttributes(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.SaveOptions):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter, options:cs.system.xml.linq.SaveOptions):Void {})
	/**
	 * Outputs this  to the specified .
	 * @param stream The stream to output this  to.
	 */
	function Save(fileName:String, options:cs.system.xml.linq.SaveOptions):Void;
	@:overload(function(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.SaveOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * @param stream 
	 * @param options 
	 * @param cancellationToken 
	 */
	function SaveAsync(textWriter:cs.system.io.TextWriter, options:cs.system.xml.linq.SaveOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Sets the value of an attribute, adds an attribute, or removes an attribute.
	 * @param name An  that contains the name of the attribute to change.
	 * @param value The value to assign to the attribute. The attribute is removed if
	 * the value is . Otherwise, the value is converted to its string representation
	 * and assigned to the  property of the attribute.
	 */
	function SetAttributeValue(name:cs.system.xml.linq.XName, value:Dynamic):Void;
	/**
	 * Sets the value of a child element, adds a child element, or removes a child
	 * element.
	 * @param name An  that contains the name of the child element to change.
	 * @param value The value to assign to the child element. The child element is
	 * removed if the value is . Otherwise, the value is converted to its string
	 * representation and assigned to the  property of the child element.
	 */
	function SetElementValue(name:cs.system.xml.linq.XName, value:Dynamic):Void;
	/**
	 * Sets the value of this element.
	 * @param value The value to assign to this element. The value is converted to its
	 * string representation and assigned to the  property.
	 */
	function SetValue(value:Dynamic):Void;
	/**
	 * Write this element to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}

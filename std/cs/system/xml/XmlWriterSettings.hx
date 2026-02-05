package cs.system.xml;

/** Specifies a set of features to support on the  object created by the  method. */
@:native("System.Xml.XmlWriterSettings")
extern class XmlWriterSettings {
	/**
	 * Gets or sets a value that indicates whether asynchronous  methods can be used on
	 * a particular  instance.
	 * @return if asynchronous methods can be used; otherwise, .
	 */
	var Async(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether the XML writer should check to
	 * ensure that all characters in the document conform to the "2.2 Characters"
	 * section of the W3C XML 1.0 Recommendation.
	 * @return to do character checking; otherwise, . The default is .
	 */
	var CheckCharacters(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether the  should also close the underlying
	 * stream or  when the  method is called.
	 * @return to also close the underlying stream or ; otherwise, . The default is .
	 */
	var CloseOutput(default, default):Bool;
	/**
	 * Gets or sets the level of conformance that the XML writer checks the XML output
	 * for.
	 * @return One of the enumeration values that specifies the level of conformance
	 * (document, fragment, or automatic detection). The default is .
	 */
	var ConformanceLevel(default, default):cs.system.xml.ConformanceLevel;
	/**
	 * Gets or sets a value that indicates whether the  does not escape URI attributes.
	 * @return if the  does not escape URI attributes; otherwise, . The default is .
	 */
	var DoNotEscapeUriAttributes(default, default):Bool;
	/**
	 * Gets or sets the type of text encoding to use.
	 * @return The text encoding to use. The default is .
	 */
	var Encoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets a value indicating whether to indent elements.
	 * @return to write individual elements on new lines and indent; otherwise, . The
	 * default is .
	 */
	var Indent(default, default):Bool;
	/**
	 * Gets or sets the character string to use when indenting. This setting is used
	 * when the  property is set to .
	 * @return The character string to use when indenting. This can be set to any
	 * string value. However, to ensure valid XML, you should specify only valid white
	 * space characters, such as space characters, tabs, carriage returns, or line
	 * feeds. The default is two spaces.
	 */
	var IndentChars(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the  should remove duplicate
	 * namespace declarations when writing XML content. The default behavior is for the
	 * writer to output all namespace declarations that are present in the writer's
	 * namespace resolver.
	 * @return The  enumeration used to specify whether to remove duplicate namespace
	 * declarations in the .
	 */
	var NamespaceHandling(default, default):cs.system.xml.NamespaceHandling;
	/**
	 * Gets or sets the character string to use for line breaks.
	 * @return The character string to use for line breaks. This can be set to any
	 * string value. However, to ensure valid XML, you should specify only valid white
	 * space characters, such as space characters, tabs, carriage returns, or line
	 * feeds. The default is \r\n (carriage return, new line).
	 */
	var NewLineChars(default, default):String;
	/**
	 * Gets or sets a value indicating whether to normalize line breaks in the output.
	 * @return One of the  values. The default is .
	 */
	var NewLineHandling(default, default):cs.system.xml.NewLineHandling;
	/**
	 * Gets or sets a value indicating whether to write attributes on a new line.
	 * @return to write attributes on individual lines; otherwise, . The default is .
	 * This setting has no effect when the  property value is . When  is set to , each
	 * attribute is pre-pended with a new line and one extra level of indentation.
	 */
	var NewLineOnAttributes(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to omit an XML declaration.
	 * @return to omit the XML declaration; otherwise, . The default is , an XML
	 * declaration is written.
	 */
	var OmitXmlDeclaration(default, default):Bool;
	/**
	 * Gets the method used to serialize the  output.
	 * @return One of the  values. The default is .
	 */
	var OutputMethod(default, never):cs.system.xml.XmlOutputMethod;
	/**
	 * Gets or sets a value that indicates whether the  will add closing tags to all
	 * unclosed element tags when the  method is called.
	 * @return if all unclosed element tags will be closed out; otherwise, . The
	 * default value is .
	 */
	var WriteEndDocumentOnClose(default, default):Bool;
	function new():Void;
	/**
	 * Creates a copy of the  instance.
	 * @return The cloned  object.
	 */
	function Clone():cs.system.xml.XmlWriterSettings;
	/** Resets the members of the settings class to their default values. */
	function Reset():Void;
}

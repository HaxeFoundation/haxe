package cs.system.xml;

/** Specifies a set of features to support on the  object created by the  method. */
@:native("System.Xml.XmlReaderSettings")
extern class XmlReaderSettings {
	/**
	 * Gets or sets whether asynchronous  methods can be used on a particular 
	 * instance.
	 * @return if asynchronous methods can be used; otherwise, .
	 */
	var Async(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to do character checking.
	 * @return to do character checking; otherwise . The default is . If the  is
	 * processing text data, it always checks that the XML names and text content are
	 * valid, regardless of the property setting. Setting  to  turns off character
	 * checking for character entity references.
	 */
	var CheckCharacters(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether the underlying stream or  should be
	 * closed when the reader is closed.
	 * @return to close the underlying stream or  when the reader is closed; otherwise
	 * . The default is .
	 */
	var CloseInput(default, default):Bool;
	/**
	 * Gets or sets the level of conformance which the  will comply.
	 * @return One of the enumeration values that specifies the level of conformance
	 * that the XML reader will enforce. The default is .
	 */
	var ConformanceLevel(default, default):cs.system.xml.ConformanceLevel;
	/**
	 * Gets or sets a value that determines the processing of DTDs.
	 * @return One of the enumeration values that determines the processing of DTDs.
	 * The default is .
	 */
	var DtdProcessing(default, default):cs.system.xml.DtdProcessing;
	/**
	 * Gets or sets a value indicating whether to ignore comments.
	 * @return to ignore comments; otherwise . The default is .
	 */
	var IgnoreComments(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to ignore processing instructions.
	 * @return to ignore processing instructions; otherwise . The default is .
	 */
	var IgnoreProcessingInstructions(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to ignore insignificant white space.
	 * @return to ignore white space; otherwise . The default is .
	 */
	var IgnoreWhitespace(default, default):Bool;
	/**
	 * Gets or sets line number offset of the  object.
	 * @return The line number offset. The default is 0.
	 */
	var LineNumberOffset(default, default):Int;
	/**
	 * Gets or sets line position offset of the  object.
	 * @return The line position offset. The default is 0.
	 */
	var LinePositionOffset(default, default):Int;
	/**
	 * Gets or sets a value indicating the maximum allowable number of characters in a
	 * document that result from expanding entities.
	 * @return The maximum allowable number of characters from expanded entities. The
	 * default is 0.
	 */
	var MaxCharactersFromEntities(default, default):haxe.Int64;
	/**
	 * Gets or sets a value indicating the maximum allowable number of characters in an
	 * XML document. A zero (0) value means no limits on the size of the XML document.
	 * A non-zero value specifies the maximum size, in characters.
	 * @return The maximum allowable number of characters in an XML document. The
	 * default is 0.
	 */
	var MaxCharactersInDocument(default, default):haxe.Int64;
	/**
	 * Gets or sets the  used for atomized string comparisons.
	 * @return The  that stores all the atomized strings used by all  instances created
	 * using this  object. The default is . The created  instance will use a new empty 
	 * if this value is .
	 */
	var NameTable(default, default):cs.system.xml.XmlNameTable;
	/**
	 * Gets or sets a value indicating whether to prohibit document type definition
	 * (DTD) processing. This property is obsolete. Use  instead.
	 * @return to prohibit DTD processing; otherwise . The default is .
	 */
	var ProhibitDtd(default, default):Bool;
	/**
	 * Gets or sets the  to use when performing schema validation.
	 * @return The  to use when performing schema validation. The default is an empty 
	 * object.
	 */
	var Schemas(default, default):cs.system.xml.schema.XmlSchemaSet;
	/**
	 * Gets or sets a value indicating the schema validation settings. This setting
	 * applies to  objects that validate schemas ( property set to ).
	 * @return A bitwise combination of enumeration values that specify validation
	 * options.  and  are enabled by default. , , and  are disabled by default.
	 */
	var ValidationFlags(default, default):cs.system.xml.schema.XmlSchemaValidationFlags;
	/**
	 * Gets or sets a value indicating whether the  will perform validation or type
	 * assignment when reading.
	 * @return One of the  values that indicates whether XmlReader will perform
	 * validation or type assignment when reading. The default is .
	 */
	var ValidationType(default, default):cs.system.xml.ValidationType;
	/**
	 * Sets the  used to access external documents.
	 * @return An  used to access external documents. If set to , an  is thrown when
	 * the  tries to access an external resource. The default is a new  with no
	 * credentials.  Starting with the .NET Framework 4.5.2, this setting has a default
	 * value of .
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	function new():Void;
	/**
	 * Creates a copy of the  instance.
	 * @return The cloned  object.
	 */
	function Clone():cs.system.xml.XmlReaderSettings;
	/** Resets the members of the settings class to their default values. */
	function Reset():Void;
}

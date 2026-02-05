package cs.system.xml.schema;

/** Represents an XML Schema Definition Language (XSD) Schema validation engine. The  class cannot be inherited. */
@:native("System.Xml.Schema.XmlSchemaValidator")
extern class XmlSchemaValidator {
	/**
	 * Gets or sets the line number information for the XML node being validated.
	 * @return An  object.
	 */
	var LineInfoProvider(default, default):cs.system.xml.IXmlLineInfo;
	/**
	 * Gets or sets the source URI for the XML node being validated.
	 * @return A  object representing the source URI for the XML node being validated;
	 * the default is .
	 */
	var SourceUri(default, default):cs.system.Uri;
	/**
	 * Gets or sets the object sent as the sender object of a validation event.
	 * @return An ; the default is this  object.
	 */
	var ValidationEventSender(default, default):Dynamic;
	/**
	 * Sets the  object used to resolve xs:import and xs:include elements as well as
	 * xsi:schemaLocation and xsi:noNamespaceSchemaLocation attributes.
	 * @return An  object; the default is an  object.
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	function new(nameTable:cs.system.xml.XmlNameTable, schemas:cs.system.xml.schema.XmlSchemaSet, namespaceResolver:cs.system.xml.IXmlNamespaceResolver, validationFlags:cs.system.xml.schema.XmlSchemaValidationFlags):Void;
	/**
	 * Adds an XML Schema Definition Language (XSD) schema to the set of schemas used
	 * for validation.
	 * @param schema An  object to add to the set of schemas used for validation.
	 */
	function AddSchema(schema:cs.system.xml.schema.XmlSchema):Void;
	/** Ends validation and checks identity constraints for the entire XML document. */
	function EndValidation():Void;
	/**
	 * Returns the expected attributes for the current element context.
	 * @return An array of  objects or an empty array if there are no expected
	 * attributes.
	 */
	function GetExpectedAttributes():cs.NativeArray<cs.system.xml.schema.XmlSchemaAttribute>;
	/**
	 * Returns the expected particles in the current element context.
	 * @return An array of  objects or an empty array if there are no expected
	 * particles.
	 */
	function GetExpectedParticles():cs.NativeArray<cs.system.xml.schema.XmlSchemaParticle>;
	/**
	 * Validates identity constraints on the default attributes and populates the 
	 * specified with  objects for any attributes with default values that have not
	 * been previously validated using the  method in the element context.
	 * @param defaultAttributes An  to populate with  objects for any attributes not
	 * yet encountered during validation in the element context.
	 */
	function GetUnspecifiedDefaultAttributes(defaultAttributes:cs.system.collections.ArrayList):Void;
	@:overload(function():Void {})
	/** Initializes the state of the  object. */
	function Initialize(partialValidationType:cs.system.xml.schema.XmlSchemaObject):Void;
	/**
	 * Skips validation of the current element content and prepares the  object to
	 * validate content in the parent element's context.
	 * @param schemaInfo An  object whose properties are set if the current element
	 * content is successfully skipped. This parameter can be .
	 */
	function SkipToEndElement(schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Void;
	@:overload(function(localName:String, namespaceUri:String, attributeValue:String, schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Dynamic {})
	/**
	 * Validates the attribute name, namespace URI, and value in the current element
	 * context.
	 * @param localName The local name of the attribute to validate.
	 * @param namespaceUri The namespace URI of the attribute to validate.
	 * @param attributeValue The value of the attribute to validate.
	 * @param schemaInfo An  object whose properties are set on successful validation
	 * of the attribute. This parameter can be .
	 * @return The validated attribute's value.
	 */
	function ValidateAttribute(localName:String, namespaceUri:String, attributeValue:cs.system.xml.schema.XmlValueGetter, schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Dynamic;
	@:overload(function(localName:String, namespaceUri:String, schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Void {})
	/**
	 * Validates the element in the current context.
	 * @param localName The local name of the element to validate.
	 * @param namespaceUri The namespace URI of the element to validate.
	 * @param schemaInfo An  object whose properties are set on successful validation
	 * of the element's name. This parameter can be .
	 */
	function ValidateElement(localName:String, namespaceUri:String, schemaInfo:cs.system.xml.schema.XmlSchemaInfo, xsiType:String, xsiNil:String, xsiSchemaLocation:String, xsiNoNamespaceSchemaLocation:String):Void;
	@:overload(function(schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Dynamic {})
	/**
	 * Verifies if the text content of the element is valid according to its data type
	 * for elements with simple content, and verifies if the content of the current
	 * element is complete for elements with complex content.
	 * @param schemaInfo An  object whose properties are set on successful validation
	 * of the element. This parameter can be .
	 * @return The parsed, typed text value of the element if the element has simple
	 * content.
	 */
	function ValidateEndElement(schemaInfo:cs.system.xml.schema.XmlSchemaInfo, typedValue:Dynamic):Dynamic;
	/**
	 * Verifies whether all the required attributes in the element context are present
	 * and prepares the  object to validate the child content of the element.
	 * @param schemaInfo An  object whose properties are set on successful verification
	 * that all the required attributes in the element context are present. This
	 * parameter can be .
	 */
	function ValidateEndOfAttributes(schemaInfo:cs.system.xml.schema.XmlSchemaInfo):Void;
	@:overload(function(elementValue:String):Void {})
	/**
	 * Validates whether the text  specified is allowed in the current element context,
	 * and accumulates the text for validation if the current element has simple
	 * content.
	 * @param elementValue A text  to validate in the current element context.
	 */
	function ValidateText(elementValue:cs.system.xml.schema.XmlValueGetter):Void;
	@:overload(function(elementValue:String):Void {})
	/**
	 * Validates whether the white space in the  specified is allowed in the current
	 * element context, and accumulates the white space for validation if the current
	 * element has simple content.
	 * @param elementValue A white space  to validate in the current element context.
	 */
	function ValidateWhitespace(elementValue:cs.system.xml.schema.XmlValueGetter):Void;
}

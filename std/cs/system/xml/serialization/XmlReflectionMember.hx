package cs.system.xml.serialization;

/** Provides mappings between code entities in .NET Framework Web service methods and the content of Web Services Description Language (WSDL) messages that are defined for SOAP Web services. */
@:native("System.Xml.Serialization.XmlReflectionMember")
extern class XmlReflectionMember {
	/**
	 * Gets or sets a value that indicates whether the  represents a Web service method
	 * return value, as opposed to an output parameter.
	 * @return , if the member represents a Web service return value; otherwise, .
	 */
	var IsReturnValue(default, default):Bool;
	/**
	 * Gets or sets the name of the Web service method member for this mapping.
	 * @return The name of the Web service method.
	 */
	var MemberName(default, default):String;
	/**
	 * Gets or sets the type of the Web service method member code entity that is
	 * represented by this mapping.
	 * @return The  of the Web service method member code entity that is represented by
	 * this mapping.
	 */
	var MemberType(default, default):cs.system.Type;
	/**
	 * Gets or sets a value that indicates that the value of the corresponding XML
	 * element definition's isNullable attribute is .
	 * @return to override the  property; otherwise, .
	 */
	var OverrideIsNullable(default, default):Bool;
	/**
	 * Gets or sets a  with the collection of SOAP-related attributes that have been
	 * applied to the member code entity.
	 * @return A  that contains the objects that represent SOAP attributes applied to
	 * the member.
	 */
	var SoapAttributes(default, default):cs.system.xml.serialization.SoapAttributes;
	/**
	 * Gets or sets an  with the collection of -related attributes that have been
	 * applied to the member code entity.
	 * @return An  that represents XML attributes that have been applied to the member
	 * code.
	 */
	var XmlAttributes(default, default):cs.system.xml.serialization.XmlAttributes;
	function new():Void;
}

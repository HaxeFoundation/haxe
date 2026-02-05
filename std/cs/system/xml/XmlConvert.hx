package cs.system.xml;

/** Encodes and decodes XML names, and provides methods for converting between common language runtime types and XML Schema definition language (XSD) types. When converting data types, the values returned are locale-independent. */
@:native("System.Xml.XmlConvert")
extern class XmlConvert {
	function new():Void;
	/**
	 * Decodes a name. This method does the reverse of the  and  methods.
	 * @param name The name to be transformed.
	 * @return The decoded name.
	 */
	static function DecodeName(name:String):String;
	/**
	 * Converts the name to a valid XML local name.
	 * @param name The name to be encoded.
	 * @return The encoded name.
	 */
	static function EncodeLocalName(name:String):String;
	/**
	 * Converts the name to a valid XML name.
	 * @param name A name to be translated.
	 * @return The name with any invalid characters replaced by an escape string.
	 */
	static function EncodeName(name:String):String;
	/**
	 * Verifies the name is valid according to the XML specification.
	 * @param name The name to be encoded.
	 * @return The encoded name.
	 */
	static function EncodeNmToken(name:String):String;
	/**
	 * Checks whether the passed-in character is a valid non-colon character type.
	 * @param ch The character to verify as a non-colon character.
	 * @return if the character is a valid non-colon character type; otherwise, .
	 */
	static function IsNCNameChar(ch:cs.Char16):Bool;
	/**
	 * Returns the passed-in character instance if the character in the argument is a
	 * valid public id character, otherwise .
	 * @param ch object to validate.
	 * @return The passed-in character if the character is a valid public id character,
	 * otherwise .
	 */
	static function IsPublicIdChar(ch:cs.Char16):Bool;
	/**
	 * Checks if the passed-in character is a valid Start Name Character type.
	 * @param ch The character to validate.
	 * @return if the character is a valid Start Name Character type; otherwise, .
	 */
	static function IsStartNCNameChar(ch:cs.Char16):Bool;
	/**
	 * Checks if the passed-in character is a valid XML whitespace character.
	 * @param ch The character to validate.
	 * @return if the passed in character is a valid XML whitespace character;
	 * otherwise, .
	 */
	static function IsWhitespaceChar(ch:cs.Char16):Bool;
	/**
	 * Checks if the passed-in character is a valid XML character.
	 * @param ch The character to validate.
	 * @return if the passed in character is a valid XML character; otherwise, .
	 */
	static function IsXmlChar(ch:cs.Char16):Bool;
	/**
	 * Checks if the passed-in surrogate pair of characters is a valid XML character.
	 * @param lowChar The surrogate character to validate.
	 * @param highChar The surrogate character to validate.
	 * @return if the passed in surrogate pair of characters is a valid XML character;
	 * otherwise, .
	 */
	static function IsXmlSurrogatePair(lowChar:cs.Char16, highChar:cs.Char16):Bool;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  value, that is,  or .
	 */
	static function ToBoolean(s:String):Bool;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToByte(s:String):cs.UInt8;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string containing a single character to convert.
	 * @return A  representing the single character.
	 */
	static function ToChar(s:String):cs.Char16;
	@:overload(function(s:String):cs.system.DateTime {})
	@:overload(function(s:String, format:String):cs.system.DateTime {})
	@:overload(function(s:String, formats:cs.NativeArray<String>):cs.system.DateTime {})
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToDateTime(s:String, dateTimeOption:cs.system.xml.XmlDateTimeSerializationMode):cs.system.DateTime;
	@:overload(function(s:String):cs.system.DateTimeOffset {})
	@:overload(function(s:String, format:String):cs.system.DateTimeOffset {})
	/**
	 * Converts the supplied  to a  equivalent.
	 * @param s The string to convert. Note The string must conform to a subset of the
	 * W3C Recommendation for the XML dateTime type. For more information, see the
	 * dateTime section of the XML Schema specification..
	 * @return The  equivalent of the supplied string.
	 */
	static function ToDateTimeOffset(s:String, formats:cs.NativeArray<String>):cs.system.DateTimeOffset;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToDecimal(s:String):cs.system.Decimal;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToDouble(s:String):Float;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToGuid(s:String):cs.system.Guid;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return An  equivalent of the string.
	 */
	static function ToInt16(s:String):cs.Int16;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return An  equivalent of the string.
	 */
	static function ToInt32(s:String):Int;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return An  equivalent of the string.
	 */
	static function ToInt64(s:String):haxe.Int64;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return An  equivalent of the string.
	 */
	static function ToSByte(s:String):cs.Int8;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToSingle(s:String):Single;
	@:overload(function(value:Bool):String {})
	@:overload(function(value:cs.UInt8):String {})
	@:overload(function(value:cs.Char16):String {})
	@:overload(function(value:cs.system.DateTime):String {})
	@:overload(function(value:cs.system.DateTimeOffset):String {})
	@:overload(function(value:cs.system.Decimal):String {})
	@:overload(function(value:Float):String {})
	@:overload(function(value:cs.system.Guid):String {})
	@:overload(function(value:cs.Int16):String {})
	@:overload(function(value:Int):String {})
	@:overload(function(value:haxe.Int64):String {})
	@:overload(function(value:cs.Int8):String {})
	@:overload(function(value:Single):String {})
	@:overload(function(value:cs.system.TimeSpan):String {})
	@:overload(function(value:cs.UInt16):String {})
	@:overload(function(value:cs.UInt):String {})
	@:overload(function(value:cs.UInt64):String {})
	@:overload(function(value:cs.system.DateTime, format:String):String {})
	@:overload(function(value:cs.system.DateTime, dateTimeOption:cs.system.xml.XmlDateTimeSerializationMode):String {})
	/**
	 * Converts the  to a .
	 * @param value The value to convert.
	 * @return A string representation of the , that is, "true" or "false".
	 */
	static function ToString(value:cs.system.DateTimeOffset, format:String):String;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert. The string format must conform to the W3C XML
	 * Schema Part 2: Datatypes recommendation for duration.
	 * @return A  equivalent of the string.
	 */
	static function ToTimeSpan(s:String):cs.system.TimeSpan;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToUInt16(s:String):cs.UInt16;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToUInt32(s:String):cs.UInt;
	/**
	 * Converts the  to a  equivalent.
	 * @param s The string to convert.
	 * @return A  equivalent of the string.
	 */
	static function ToUInt64(s:String):cs.UInt64;
	/**
	 * Verifies that the name is a valid name according to the W3C Extended Markup
	 * Language recommendation.
	 * @param name The name to verify.
	 * @return The name, if it is a valid XML name.
	 */
	static function VerifyName(name:String):String;
	/**
	 * Verifies that the name is a valid  according to the W3C Extended Markup Language
	 * recommendation. An  is a name that cannot contain a colon.
	 * @param name The name to verify.
	 * @return The name, if it is a valid NCName.
	 */
	static function VerifyNCName(name:String):String;
	/**
	 * Verifies that the string is a valid NMTOKEN according to the W3C XML Schema
	 * Part2: Datatypes recommendation
	 * @param name The string you wish to verify.
	 * @return The name token, if it is a valid NMTOKEN.
	 */
	static function VerifyNMTOKEN(name:String):String;
	/**
	 * Returns the passed in string instance if all the characters in the string
	 * argument are valid public id characters.
	 * @param publicId that contains the id to validate.
	 * @return The passed-in string if all the characters in the argument are valid
	 * public id characters.
	 */
	static function VerifyPublicId(publicId:String):String;
	/**
	 * Verifies that the string is a valid token according to the W3C XML Schema Part2:
	 * Datatypes recommendation.
	 * @param token The string value you wish to verify.
	 * @return The token, if it is a valid token.
	 */
	static function VerifyTOKEN(token:String):String;
	/**
	 * Returns the passed-in string instance if all the characters in the string
	 * argument are valid whitespace characters.
	 * @param content to verify.
	 * @return The passed-in string instance if all the characters in the string
	 * argument are valid whitespace characters, otherwise .
	 */
	static function VerifyWhitespace(content:String):String;
	/**
	 * Returns the passed-in string if all the characters and surrogate pair characters
	 * in the string argument are valid XML characters, otherwise an  is thrown with
	 * information on the first invalid character encountered.
	 * @param content that contains characters to verify.
	 * @return The passed-in string if all the characters and surrogate-pair characters
	 * in the string argument are valid XML characters, otherwise an  is thrown with
	 * information on the first invalid character encountered.
	 */
	static function VerifyXmlChars(content:String):String;
}

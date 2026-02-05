package cs.system.xml.serialization;

/** Provides static methods to convert input text into names for code entities. */
@:native("System.Xml.Serialization.CodeIdentifier")
extern class CodeIdentifier {
	function new():Void;
	/**
	 * Produces a camel-case string from an input string.
	 * @param identifier The name of a code entity, such as a method parameter,
	 * typically taken from an XML element or attribute name.
	 * @return A camel-case version of the parameter string.
	 */
	static function MakeCamel(identifier:String):String;
	/**
	 * Produces a Pascal-case string from an input string.
	 * @param identifier The name of a code entity, such as a method parameter,
	 * typically taken from an XML element or attribute name.
	 * @return A Pascal-case version of the parameter string.
	 */
	static function MakePascal(identifier:String):String;
	/**
	 * Produces a valid code entity name from an input string.
	 * @param identifier The name of a code entity, such as a method parameter,
	 * typically taken from an XML element or attribute name.
	 * @return A string that can be used as a code identifier, such as the name of a
	 * method parameter.
	 */
	static function MakeValid(identifier:String):String;
}

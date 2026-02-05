package cs.system.xml.schema;

/**
 * A  used by the  class to pass attribute, text, and white space values as a
 * Common Language Runtime (CLR) type compatible with the XML Schema Definition
 * Language (XSD) type of the attribute, text, or white space.
 * @return An object containing the attribute, text, or white space value. The
 * object is a CLR type that corresponds to the XSD type of the attribute, text, or
 * white space value.
 */
@:native("System.Xml.Schema.XmlValueGetter")
extern class XmlValueGetter extends cs.system.MulticastDelegate {
	function new(func:()->Dynamic):Void;
	function Invoke():Dynamic;
}

package cs.system.xml.schema;

/** An abstract class for , , or . */
@:native("System.Xml.Schema.XmlSchemaGroupBase")
extern class XmlSchemaGroupBase extends cs.system.xml.schema.XmlSchemaParticle {
	/**
	 * This collection is used to add new elements to the compositor.
	 * @return An .
	 */
	var Items(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
}

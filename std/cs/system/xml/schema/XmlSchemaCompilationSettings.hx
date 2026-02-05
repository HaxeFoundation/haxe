package cs.system.xml.schema;

/** Provides schema compilation options for the  class This class cannot be inherited. */
@:native("System.Xml.Schema.XmlSchemaCompilationSettings")
extern class XmlSchemaCompilationSettings {
	/**
	 * Gets or sets a value indicating whether the  should check for Unique Particle
	 * Attribution (UPA) violations.
	 * @return if the  should check for Unique Particle Attribution (UPA) violations;
	 * otherwise, . The default is .
	 */
	var EnableUpaCheck(default, default):Bool;
	function new():Void;
}

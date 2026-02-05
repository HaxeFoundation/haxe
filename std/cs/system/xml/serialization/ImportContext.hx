package cs.system.xml.serialization;

/** Describes the context in which a set of schema is bound to .NET Framework code entities. */
@:native("System.Xml.Serialization.ImportContext")
extern class ImportContext {
	/**
	 * Gets a value that determines whether custom types are shared.
	 * @return , if custom types are shared among schema; otherwise, .
	 */
	var ShareTypes(default, never):Bool;
	/**
	 * Gets a set of code entities to which the context applies.
	 * @return A  that specifies the code entities to which the context applies.
	 */
	var TypeIdentifiers(default, never):cs.system.xml.serialization.CodeIdentifiers;
	/**
	 * Gets a collection of warnings that are generated when importing the code entity
	 * descriptions.
	 * @return A  that contains warnings that were generated when importing the code
	 * entity descriptions.
	 */
	var Warnings(default, never):cs.system.collections.specialized.StringCollection;
	function new(identifiers:cs.system.xml.serialization.CodeIdentifiers, shareTypes:Bool):Void;
}

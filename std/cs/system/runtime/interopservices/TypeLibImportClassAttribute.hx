package cs.system.runtime.interopservices;

/** Specifies which  exclusively uses an interface. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.TypeLibImportClassAttribute")
extern class TypeLibImportClassAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of a  object that exclusively uses an interface.
	 * @return The name of a  object that exclusively uses an interface.
	 */
	var Value(default, never):String;
	function new(importClass:cs.system.Type):Void;
}

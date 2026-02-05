package cs.system.runtime.serialization;

/** Represents the options that can be set for an . */
@:native("System.Runtime.Serialization.ExportOptions")
extern class ExportOptions {
	/**
	 * Gets the collection of types that may be encountered during serialization or
	 * deserialization.
	 * @return A  collection that contains types that may be encountered during
	 * serialization or deserialization. XML schema representations are exported for
	 * all the types specified in this collection by the .
	 */
	var KnownTypes(default, never):cs.system.collections.objectmodel.Collection<cs.system.Type>;
	function new():Void;
}

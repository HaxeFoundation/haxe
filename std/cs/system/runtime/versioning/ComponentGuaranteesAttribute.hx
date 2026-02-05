package cs.system.runtime.versioning;

/** Defines the compatibility guarantee of a component, type, or type member that may span multiple versions. */
@:native("System.Runtime.Versioning.ComponentGuaranteesAttribute")
extern class ComponentGuaranteesAttribute extends cs.system.Attribute {
	/**
	 * Gets a value that indicates the guaranteed level of compatibility of a library,
	 * type, or type member that spans multiple versions.
	 * @return One of the enumeration values that specifies the level of compatibility
	 * that is guaranteed across multiple versions.
	 */
	var Guarantees(default, never):cs.system.runtime.versioning.ComponentGuaranteesOptions;
	function new(guarantees:cs.system.runtime.versioning.ComponentGuaranteesOptions):Void;
}

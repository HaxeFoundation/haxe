package cs.system.runtime.versioning;

/** Specifies the resource consumed by the member of a class. This class cannot be inherited. */
@:native("System.Runtime.Versioning.ResourceConsumptionAttribute")
extern class ResourceConsumptionAttribute extends cs.system.Attribute {
	/**
	 * Gets the consumption scope for this member.
	 * @return A  object specifying the resource scope used by this member.
	 */
	var ConsumptionScope(default, never):cs.system.runtime.versioning.ResourceScope;
	/**
	 * Gets the resource scope for the consumed resource.
	 * @return A  object specifying the resource scope of the consumed member.
	 */
	var ResourceScope(default, never):cs.system.runtime.versioning.ResourceScope;
	@:overload(function(resourceScope:cs.system.runtime.versioning.ResourceScope):Void {})
	function new(resourceScope:cs.system.runtime.versioning.ResourceScope, consumptionScope:cs.system.runtime.versioning.ResourceScope):Void;
}

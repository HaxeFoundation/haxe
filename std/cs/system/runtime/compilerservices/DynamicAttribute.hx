package cs.system.runtime.compilerservices;

/** Indicates that the use of  on a member is meant to be treated as a dynamically dispatched type. */
@:native("System.Runtime.CompilerServices.DynamicAttribute")
extern class DynamicAttribute extends cs.system.Attribute {
	/**
	 * Specifies, in a prefix traversal of a type's construction, which  occurrences
	 * are meant to be treated as a dynamically dispatched type.
	 * @return The list of  occurrences that are meant to be treated as a dynamically
	 * dispatched type.
	 */
	var TransformFlags(default, never):cs.system.collections.generic.IList<Bool>;
	@:overload(function():Void {})
	function new(transformFlags:cs.NativeArray<Bool>):Void;
}

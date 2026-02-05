package cs.system.runtime.compilerservices;

/** Indicates that the use of a value tuple on a member is meant to be treated as a tuple with element names. */
@:native("System.Runtime.CompilerServices.TupleElementNamesAttribute")
extern class TupleElementNamesAttribute extends cs.system.Attribute {
	/**
	 * Specifies, in a pre-order depth-first traversal of a type's construction, which
	 * value tuple elements are meant to carry element names.
	 * @return An array that indicates which value tuple elements are meant to carry
	 * element names.
	 */
	var TransformNames(default, never):cs.system.collections.generic.IList<String>;
	function new(transformNames:cs.NativeArray<String>):Void;
}

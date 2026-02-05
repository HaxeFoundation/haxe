package cs.system.runtime.compilerservices;

/** Specifies that an importing compiler must fully understand the semantics of a type definition, or refuse to use it.  This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.RequiredAttributeAttribute")
extern class RequiredAttributeAttribute extends cs.system.Attribute {
	/**
	 * Gets a type that an importing compiler must fully understand.
	 * @return A type that an importing compiler must fully understand.
	 */
	var RequiredContract(default, never):cs.system.Type;
	function new(requiredContract:cs.system.Type):Void;
}

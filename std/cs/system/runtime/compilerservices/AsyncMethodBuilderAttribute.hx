package cs.system.runtime.compilerservices;

/** Indicates the type of the async method builder that should be used by a language compiler to build the attributed type when used as the return type of an async method. */
@:native("System.Runtime.CompilerServices.AsyncMethodBuilderAttribute")
extern class AsyncMethodBuilderAttribute extends cs.system.Attribute {
	/**
	 * Gets the type of the associated builder.
	 * @return The type of the associated builder.
	 */
	var BuilderType(default, never):cs.system.Type;
	function new(builderType:cs.system.Type):Void;
}

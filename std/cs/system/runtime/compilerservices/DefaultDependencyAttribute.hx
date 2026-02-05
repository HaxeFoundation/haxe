package cs.system.runtime.compilerservices;

/** Provides a hint to the common language runtime (CLR) indicating how likely a dependency is to be loaded. This class is used in a dependent assembly to indicate what hint should be used when the parent does not specify the  attribute.  This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.DefaultDependencyAttribute")
extern class DefaultDependencyAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value that indicates when an assembly loads a dependency.
	 * @return One of the  values.
	 */
	var LoadHint(default, never):cs.system.runtime.compilerservices.LoadHint;
	function new(loadHintArgument:cs.system.runtime.compilerservices.LoadHint):Void;
}

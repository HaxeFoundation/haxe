package cs.system.runtime.compilerservices;

/** Indicates when a dependency is to be loaded by the referring assembly. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.DependencyAttribute")
extern class DependencyAttribute extends cs.system.Attribute {
	/**
	 * Gets the value of the dependent assembly.
	 * @return The name of the dependent assembly.
	 */
	var DependentAssembly(default, never):String;
	/**
	 * Gets the  value that indicates when an assembly is to load a dependency.
	 * @return One of the  values.
	 */
	var LoadHint(default, never):cs.system.runtime.compilerservices.LoadHint;
	function new(dependentAssemblyArgument:String, loadHintArgument:cs.system.runtime.compilerservices.LoadHint):Void;
}

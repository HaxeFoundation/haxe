package cs.system.reflection;

/** Specifies an algorithm to hash all files in an assembly. This class cannot be inherited. */
@:native("System.Reflection.AssemblyAlgorithmIdAttribute")
extern class AssemblyAlgorithmIdAttribute extends cs.system.Attribute {
	/**
	 * Gets the hash algorithm of an assembly manifest's contents.
	 * @return An unsigned integer representing the assembly hash algorithm.
	 */
	var AlgorithmId(default, never):cs.UInt;
	@:overload(function(algorithmId:cs.system.configuration.assemblies.AssemblyHashAlgorithm):Void {})
	function new(algorithmId:cs.UInt):Void;
}

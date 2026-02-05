package cs.system.runtime.compilerservices;

/** Generates debug information for lambda expressions in an expression tree. */
@:native("System.Runtime.CompilerServices.DebugInfoGenerator")
extern class DebugInfoGenerator {
	/**
	 * Creates a program database (PDB) symbol generator.
	 * @return A PDB symbol generator.
	 */
	static function CreatePdbGenerator():cs.system.runtime.compilerservices.DebugInfoGenerator;
	/**
	 * Marks a sequence point in Microsoft intermediate language (MSIL) code.
	 * @param method The lambda expression that is generated.
	 * @param ilOffset The offset within MSIL code at which to mark the sequence point.
	 * @param sequencePoint Debug information that corresponds to the sequence point.
	 */
	function MarkSequencePoint(method:cs.system.linq.expressions.LambdaExpression, ilOffset:Int, sequencePoint:cs.system.linq.expressions.DebugInfoExpression):Void;
}

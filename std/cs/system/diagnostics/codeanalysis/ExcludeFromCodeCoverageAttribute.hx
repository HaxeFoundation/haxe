package cs.system.diagnostics.codeanalysis;

/** Specifies that the attributed code should be excluded from code coverage information. */
@:native("System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute")
extern class ExcludeFromCodeCoverageAttribute extends cs.system.Attribute {
	function new():Void;
}

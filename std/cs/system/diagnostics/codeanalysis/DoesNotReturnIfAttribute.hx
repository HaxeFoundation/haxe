package cs.system.diagnostics.codeanalysis;

@:native("System.Diagnostics.CodeAnalysis.DoesNotReturnIfAttribute")
extern class DoesNotReturnIfAttribute extends cs.system.Attribute {
	var ParameterValue(default, never):Bool;
	function new(parameterValue:Bool):Void;
}

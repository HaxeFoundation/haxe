package cs.system.diagnostics.codeanalysis;

@:native("System.Diagnostics.CodeAnalysis.NotNullIfNotNullAttribute")
extern class NotNullIfNotNullAttribute extends cs.system.Attribute {
	var ParameterName(default, never):String;
	function new(parameterName:String):Void;
}

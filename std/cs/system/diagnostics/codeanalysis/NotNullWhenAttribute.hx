package cs.system.diagnostics.codeanalysis;

@:native("System.Diagnostics.CodeAnalysis.NotNullWhenAttribute")
extern class NotNullWhenAttribute extends cs.system.Attribute {
	var ReturnValue(default, never):Bool;
	function new(returnValue:Bool):Void;
}

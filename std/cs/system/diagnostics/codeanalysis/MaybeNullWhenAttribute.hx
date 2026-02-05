package cs.system.diagnostics.codeanalysis;

@:native("System.Diagnostics.CodeAnalysis.MaybeNullWhenAttribute")
extern class MaybeNullWhenAttribute extends cs.system.Attribute {
	var ReturnValue(default, never):Bool;
	function new(returnValue:Bool):Void;
}

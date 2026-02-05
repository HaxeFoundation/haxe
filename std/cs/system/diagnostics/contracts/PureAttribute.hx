package cs.system.diagnostics.contracts;

/** Indicates that a type or method is pure, that is, it does not make any visible state changes. */
@:native("System.Diagnostics.Contracts.PureAttribute")
extern class PureAttribute extends cs.system.Attribute {
	function new():Void;
}

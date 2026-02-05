package cs.system.runtime.compilerservices;

/** Persists an 8-byte  constant for a field or parameter. */
@:native("System.Runtime.CompilerServices.DateTimeConstantAttribute")
extern class DateTimeConstantAttribute extends cs.system.runtime.compilerservices.CustomConstantAttribute {
	function new(ticks:haxe.Int64):Void;
}

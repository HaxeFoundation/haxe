package cs.system.reflection;

/** Attaches a modifier to parameters so that binding can work with parameter signatures in which the types have been modified. */
@:native("System.Reflection.ParameterModifier")
extern class ParameterModifier extends cs.system.ValueType {
	@:native("get_Item")
	function get_Item(index0:Int):Bool;
	@:native("set_Item")
	function set_Item(index0:Int, value:Bool):Void;
	function new(parameterCount:Int):Void;
}

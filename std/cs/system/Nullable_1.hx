package cs.system;

/** Supports a value type that can be assigned . This class cannot be inherited. */
@:native("System.Nullable`1")
extern class Nullable_1<T> extends cs.system.ValueType {
	var HasValue(default, never):Bool;
	var Value(default, never):T;
	function new(value:T):Void;
	static function op_Explicit<T>(value:Null<T>):T;
	static function op_Implicit<T>(value:T):Null<T>;
	/**
	 * Indicates whether two specified  objects are equal.
	 * @param T The underlying value type of the  and  parameters.
	 * @param n1 A  object.
	 * @param n2 A  object.
	 * @return if the  parameter is equal to the  parameter; otherwise, . The return
	 * value depends on the  and  properties of the two parameters that are compared.
	 * Return Value Description The  properties for  and  are . -or- The  properties
	 * for  and  are , and the  properties of the parameters are equal. The  property
	 * is  for one parameter and  for the other parameter. -or- The  properties for 
	 * and  are , and the  properties of the parameters are unequal.
	 */
	function Equals(other:Dynamic):Bool;
	function GetHashCode():Int;
	@:overload(function():T {})
	function GetValueOrDefault(defaultValue:T):T;
	function ToString():String;
}

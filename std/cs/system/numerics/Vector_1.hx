package cs.system.numerics;

/** Provides a collection of static convenience methods for creating, manipulating, combining, and converting generic vectors. */
@:native("System.Numerics.Vector`1")
extern class Vector_1<T> extends cs.system.ValueType {
	static var Count(default, never):Int;
	static var One(default, never):cs.system.numerics.Vector_1<Dynamic>;
	static var Zero(default, never):cs.system.numerics.Vector_1<Dynamic>;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:overload(function(values:cs.system.Span<T>):Void {})
	@:overload(function(value:T):Void {})
	@:overload(function(values:cs.NativeArray<T>):Void {})
	function new(values:cs.NativeArray<T>, index:Int):Void;
	static function op_Addition<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_BitwiseAnd<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_BitwiseOr<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_Division<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_Equality<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	static function op_ExclusiveOr<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt8> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Float> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.Int16> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.Int8> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Single> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt16> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt> {})
	static function op_Explicit<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt64>;
	static function op_Inequality<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	@:overload(function<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T> {})
	@:overload(function<T>(value:cs.system.numerics.Vector_1<T>, factor:T):cs.system.numerics.Vector_1<T> {})
	static function op_Multiply<T>(factor:T, value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_OnesComplement<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_Subtraction<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	static function op_UnaryNegation<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function(destination:cs.NativeArray<T>):Void {})
	function CopyTo(destination:cs.NativeArray<T>, startIndex:Int):Void;
	@:overload(function(other:cs.system.numerics.Vector_1<T>):Bool {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in two
	 * specified double-precision vectors are equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	@:overload(function():String {})
	@:overload(function(format:String):String {})
	function ToString(format:String, formatProvider:cs.system.IFormatProvider):String;
}

package cs.system.numerics;

/** Represents a vector with two single-precision floating-point values. */
@:native("System.Numerics.Vector2")
extern class Vector2 extends cs.system.ValueType {
	/**
	 * Gets a vector whose 2 elements are equal to one.
	 * @return A vector whose two elements are equal to one (that is, it returns the
	 * vector (1,1).
	 */
	static var One(default, never):cs.system.numerics.Vector2;
	/**
	 * Gets the vector (1,0).
	 * @return The vector (1,0).
	 */
	static var UnitX(default, never):cs.system.numerics.Vector2;
	/**
	 * Gets the vector (0,1).
	 * @return The vector (0,1).
	 */
	static var UnitY(default, never):cs.system.numerics.Vector2;
	/**
	 * Returns a vector whose 2 elements are equal to zero.
	 * @return A vector whose two elements are equal to zero (that is, it returns the
	 * vector (0,0).
	 */
	static var Zero(default, never):cs.system.numerics.Vector2;
	/** The X component of the vector. */
	var X:Single;
	/** The Y component of the vector. */
	var Y:Single;
	@:overload(function(value:Single):Void {})
	function new(x:Single, y:Single):Void;
	/**
	 * Returns a vector whose elements are the absolute values of each of the specified
	 * vector's elements.
	 * @param value A vector.
	 * @return The absolute value vector.
	 */
	static function Abs(value:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Adds two vectors together.
	 * @param left The first vector to add.
	 * @param right The second vector to add.
	 * @return The summed vector.
	 */
	static function Add(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Restricts a vector between a minimum and a maximum value.
	 * @param value1 The vector to restrict.
	 * @param min The minimum value.
	 * @param max The maximum value.
	 * @return The restricted vector.
	 */
	static function Clamp(value1:cs.system.numerics.Vector2, min:cs.system.numerics.Vector2, max:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Computes the Euclidean distance between the two given points.
	 * @param value1 The first point.
	 * @param value2 The second point.
	 * @return The distance.
	 */
	static function Distance(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2):Single;
	/**
	 * Returns the Euclidean distance squared between two specified points.
	 * @param value1 The first point.
	 * @param value2 The second point.
	 * @return The distance squared.
	 */
	static function DistanceSquared(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2):Single;
	@:overload(function(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2 {})
	/**
	 * Divides the first vector by the second.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector resulting from the division.
	 */
	static function Divide(left:cs.system.numerics.Vector2, divisor:Single):cs.system.numerics.Vector2;
	/**
	 * Returns the dot product of two vectors.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @return The dot product.
	 */
	static function Dot(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2):Single;
	/**
	 * Performs a linear interpolation between two vectors based on the given
	 * weighting.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @param amount A value between 0 and 1 that indicates the weight of .
	 * @return The interpolated vector.
	 */
	static function Lerp(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2, amount:Single):cs.system.numerics.Vector2;
	/**
	 * Returns a vector whose elements are the maximum of each of the pairs of elements
	 * in two specified vectors.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @return The maximized vector.
	 */
	static function Max(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Returns a vector whose elements are the minimum of each of the pairs of elements
	 * in two specified vectors.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @return The minimized vector.
	 */
	static function Min(value1:cs.system.numerics.Vector2, value2:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	@:overload(function(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2 {})
	@:overload(function(left:cs.system.numerics.Vector2, right:Single):cs.system.numerics.Vector2 {})
	/**
	 * Returns a new vector whose values are the product of each pair of elements in
	 * two specified vectors.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The element-wise product vector.
	 */
	static function Multiply(left:Single, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Negates a specified vector.
	 * @param value The vector to negate.
	 * @return The negated vector.
	 */
	static function Negate(value:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Returns a vector with the same direction as the specified vector, but with a
	 * length of one.
	 * @param value The vector to normalize.
	 * @return The normalized vector.
	 */
	static function Normalize(value:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Adds two vectors together.
	 * @param left The first vector to add.
	 * @param right The second vector to add.
	 * @return The summed vector.
	 */
	static function op_Addition(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	@:overload(function(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2 {})
	/**
	 * Divides the first vector by the second.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector that results from dividing  by .
	 */
	static function op_Division(value1:cs.system.numerics.Vector2, value2:Single):cs.system.numerics.Vector2;
	/**
	 * Returns a value that indicates whether each pair of elements in two specified
	 * vectors is equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):Bool;
	/**
	 * Returns a value that indicates whether two specified vectors are not equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):Bool;
	@:overload(function(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2 {})
	@:overload(function(left:cs.system.numerics.Vector2, right:Single):cs.system.numerics.Vector2 {})
	/**
	 * Returns a new vector whose values are the product of each pair of elements in
	 * two specified vectors.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The element-wise product vector.
	 */
	static function op_Multiply(left:Single, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Subtracts the second vector from the first.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector that results from subtracting  from .
	 */
	static function op_Subtraction(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Negates the specified vector.
	 * @param value The vector to negate.
	 * @return The negated vector.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Returns the reflection of a vector off a surface that has the specified normal.
	 * @param vector The source vector.
	 * @param normal The normal of the surface being reflected off.
	 * @return The reflected vector.
	 */
	static function Reflect(vector:cs.system.numerics.Vector2, normal:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Returns a vector whose elements are the square root of each of a specified
	 * vector's elements.
	 * @param value A vector.
	 * @return The square root vector.
	 */
	static function SquareRoot(value:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	/**
	 * Subtracts the second vector from the first.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The difference vector.
	 */
	static function Subtract(left:cs.system.numerics.Vector2, right:cs.system.numerics.Vector2):cs.system.numerics.Vector2;
	@:overload(function(position:cs.system.numerics.Vector2, matrix:cs.system.numerics.Matrix3x2):cs.system.numerics.Vector2 {})
	@:overload(function(position:cs.system.numerics.Vector2, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Vector2 {})
	/**
	 * Transforms a vector by a specified 3x2 matrix.
	 * @param position The vector to transform.
	 * @param matrix The transformation matrix.
	 * @return The transformed vector.
	 */
	static function Transform(value:cs.system.numerics.Vector2, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Vector2;
	@:overload(function(normal:cs.system.numerics.Vector2, matrix:cs.system.numerics.Matrix3x2):cs.system.numerics.Vector2 {})
	/**
	 * Transforms a vector normal by the given 3x2 matrix.
	 * @param normal The source vector.
	 * @param matrix The matrix.
	 * @return The transformed vector.
	 */
	static function TransformNormal(normal:cs.system.numerics.Vector2, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Vector2;
	@:overload(function(array:cs.NativeArray<Single>):Void {})
	/**
	 * Copies the elements of the vector to a specified array.
	 * @param array The destination array.
	 */
	function CopyTo(array:cs.NativeArray<Single>, index:Int):Void;
	@:overload(function(other:cs.system.numerics.Vector2):Bool {})
	/**
	 * Returns a value that indicates whether this instance and another vector are
	 * equal.
	 * @param other The other vector.
	 * @return if the two vectors are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the length of the vector.
	 * @return The vector's length.
	 */
	function Length():Single;
	/**
	 * Returns the length of the vector squared.
	 * @return The vector's length squared.
	 */
	function LengthSquared():Single;
	@:overload(function():String {})
	@:overload(function(format:String):String {})
	/**
	 * Returns the string representation of the current instance using default
	 * formatting.
	 * @return The string representation of the current instance.
	 */
	function ToString(format:String, formatProvider:cs.system.IFormatProvider):String;
}

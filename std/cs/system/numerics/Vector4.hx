package cs.system.numerics;

/** Represents a vector with four single-precision floating-point values. */
@:native("System.Numerics.Vector4")
extern class Vector4 extends cs.system.ValueType {
	/**
	 * Gets a vector whose 4 elements are equal to one.
	 * @return Returns .
	 */
	static var One(default, never):cs.system.numerics.Vector4;
	/**
	 * Gets the vector (0,0,0,1).
	 * @return The vector (0,0,0,1).
	 */
	static var UnitW(default, never):cs.system.numerics.Vector4;
	/**
	 * Gets the vector (1,0,0,0).
	 * @return The vector (1,0,0,0).
	 */
	static var UnitX(default, never):cs.system.numerics.Vector4;
	/**
	 * Gets the vector (0,1,0,0).
	 * @return The vector (0,1,0,0).
	 */
	static var UnitY(default, never):cs.system.numerics.Vector4;
	/**
	 * Gets the vector (0,0,1,0).
	 * @return The vector (0,0,1,0).
	 */
	static var UnitZ(default, never):cs.system.numerics.Vector4;
	/**
	 * Gets a vector whose 4 elements are equal to zero.
	 * @return A vector whose four elements are equal to zero (that is, it returns the
	 * vector (0,0,0,0).
	 */
	static var Zero(default, never):cs.system.numerics.Vector4;
	/** The W component of the vector. */
	var W:Single;
	/** The X component of the vector. */
	var X:Single;
	/** The Y component of the vector. */
	var Y:Single;
	/** The Z component of the vector. */
	var Z:Single;
	@:overload(function(value:Single):Void {})
	@:overload(function(value:cs.system.numerics.Vector3, w:Single):Void {})
	@:overload(function(value:cs.system.numerics.Vector2, z:Single, w:Single):Void {})
	function new(x:Single, y:Single, z:Single, w:Single):Void;
	/**
	 * Returns a vector whose elements are the absolute values of each of the specified
	 * vector's elements.
	 * @param value A vector.
	 * @return The absolute value vector.
	 */
	static function Abs(value:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Adds two vectors together.
	 * @param left The first vector to add.
	 * @param right The second vector to add.
	 * @return The summed vector.
	 */
	static function Add(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Restricts a vector between a minimum and a maximum value.
	 * @param value1 The vector to restrict.
	 * @param min The minimum value.
	 * @param max The maximum value.
	 * @return The restricted vector.
	 */
	static function Clamp(value1:cs.system.numerics.Vector4, min:cs.system.numerics.Vector4, max:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Computes the Euclidean distance between the two given points.
	 * @param value1 The first point.
	 * @param value2 The second point.
	 * @return The distance.
	 */
	static function Distance(value1:cs.system.numerics.Vector4, value2:cs.system.numerics.Vector4):Single;
	/**
	 * Returns the Euclidean distance squared between two specified points.
	 * @param value1 The first point.
	 * @param value2 The second point.
	 * @return The distance squared.
	 */
	static function DistanceSquared(value1:cs.system.numerics.Vector4, value2:cs.system.numerics.Vector4):Single;
	@:overload(function(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4 {})
	/**
	 * Divides the first vector by the second.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector resulting from the division.
	 */
	static function Divide(left:cs.system.numerics.Vector4, divisor:Single):cs.system.numerics.Vector4;
	/**
	 * Returns the dot product of two vectors.
	 * @param vector1 The first vector.
	 * @param vector2 The second vector.
	 * @return The dot product.
	 */
	static function Dot(vector1:cs.system.numerics.Vector4, vector2:cs.system.numerics.Vector4):Single;
	/**
	 * Performs a linear interpolation between two vectors based on the given
	 * weighting.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @param amount A value between 0 and 1 that indicates the weight of .
	 * @return The interpolated vector.
	 */
	static function Lerp(value1:cs.system.numerics.Vector4, value2:cs.system.numerics.Vector4, amount:Single):cs.system.numerics.Vector4;
	/**
	 * Returns a vector whose elements are the maximum of each of the pairs of elements
	 * in two specified vectors.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @return The maximized vector.
	 */
	static function Max(value1:cs.system.numerics.Vector4, value2:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Returns a vector whose elements are the minimum of each of the pairs of elements
	 * in two specified vectors.
	 * @param value1 The first vector.
	 * @param value2 The second vector.
	 * @return The minimized vector.
	 */
	static function Min(value1:cs.system.numerics.Vector4, value2:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	@:overload(function(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4 {})
	@:overload(function(left:cs.system.numerics.Vector4, right:Single):cs.system.numerics.Vector4 {})
	/**
	 * Returns a new vector whose values are the product of each pair of elements in
	 * two specified vectors.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The element-wise product vector.
	 */
	static function Multiply(left:Single, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Negates a specified vector.
	 * @param value The vector to negate.
	 * @return The negated vector.
	 */
	static function Negate(value:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Returns a vector with the same direction as the specified vector, but with a
	 * length of one.
	 * @param vector The vector to normalize.
	 * @return The normalized vector.
	 */
	static function Normalize(vector:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Adds two vectors together.
	 * @param left The first vector to add.
	 * @param right The second vector to add.
	 * @return The summed vector.
	 */
	static function op_Addition(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	@:overload(function(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4 {})
	/**
	 * Divides the first vector by the second.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector that results from dividing  by .
	 */
	static function op_Division(value1:cs.system.numerics.Vector4, value2:Single):cs.system.numerics.Vector4;
	/**
	 * Returns a value that indicates whether each pair of elements in two specified
	 * vectors is equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):Bool;
	/**
	 * Returns a value that indicates whether two specified vectors are not equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):Bool;
	@:overload(function(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4 {})
	@:overload(function(left:cs.system.numerics.Vector4, right:Single):cs.system.numerics.Vector4 {})
	/**
	 * Returns a new vector whose values are the product of each pair of elements in
	 * two specified vectors.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The element-wise product vector.
	 */
	static function op_Multiply(left:Single, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Subtracts the second vector from the first.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The vector that results from subtracting  from .
	 */
	static function op_Subtraction(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Negates the specified vector.
	 * @param value The vector to negate.
	 * @return The negated vector.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Returns a vector whose elements are the square root of each of a specified
	 * vector's elements.
	 * @param value A vector.
	 * @return The square root vector.
	 */
	static function SquareRoot(value:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	/**
	 * Subtracts the second vector from the first.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The difference vector.
	 */
	static function Subtract(left:cs.system.numerics.Vector4, right:cs.system.numerics.Vector4):cs.system.numerics.Vector4;
	@:overload(function(position:cs.system.numerics.Vector2, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Vector4 {})
	@:overload(function(value:cs.system.numerics.Vector2, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Vector4 {})
	@:overload(function(position:cs.system.numerics.Vector3, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Vector4 {})
	@:overload(function(value:cs.system.numerics.Vector3, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Vector4 {})
	@:overload(function(vector:cs.system.numerics.Vector4, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Vector4 {})
	/**
	 * Transforms a two-dimensional vector by a specified 4x4 matrix.
	 * @param position The vector to transform.
	 * @param matrix The transformation matrix.
	 * @return The transformed vector.
	 */
	static function Transform(value:cs.system.numerics.Vector4, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Vector4;
	@:overload(function(array:cs.NativeArray<Single>):Void {})
	/**
	 * Copies the elements of the vector to a specified array.
	 * @param array The destination array.
	 */
	function CopyTo(array:cs.NativeArray<Single>, index:Int):Void;
	@:overload(function(other:cs.system.numerics.Vector4):Bool {})
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
	 * Returns the length of this vector object.
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

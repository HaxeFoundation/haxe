package cs.system.numerics;

/** Represents a vector that is used to encode three-dimensional physical rotations. */
@:native("System.Numerics.Quaternion")
extern class Quaternion extends cs.system.ValueType {
	/**
	 * Gets a quaternion that represents no rotation.
	 * @return A quaternion whose values are (0, 0, 0, 1).
	 */
	static var Identity(default, never):cs.system.numerics.Quaternion;
	/** The rotation component of the quaternion. */
	var W:Single;
	/** The X value of the vector component of the quaternion. */
	var X:Single;
	/** The Y value of the vector component of the quaternion. */
	var Y:Single;
	/** The Z value of the vector component of the quaternion. */
	var Z:Single;
	/**
	 * Gets a value that indicates whether the current instance is the identity
	 * quaternion.
	 * @return if the current instance is the identity quaternion; otherwise, .
	 */
	var IsIdentity(default, never):Bool;
	@:overload(function(vectorPart:cs.system.numerics.Vector3, scalarPart:Single):Void {})
	function new(x:Single, y:Single, z:Single, w:Single):Void;
	/**
	 * Adds each element in one quaternion with its corresponding element in a second
	 * quaternion.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The quaternion that contains the summed values of  and .
	 */
	static function Add(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Concatenates two quaternions.
	 * @param value1 The first quaternion rotation in the series.
	 * @param value2 The second quaternion rotation in the series.
	 * @return A new quaternion representing the concatenation of the  rotation
	 * followed by the  rotation.
	 */
	static function Concatenate(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Returns the conjugate of a specified quaternion.
	 * @param value The quaternion.
	 * @return A new quaternion that is the conjugate of .
	 */
	static function Conjugate(value:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Creates a quaternion from a unit vector and an angle to rotate around the
	 * vector.
	 * @param axis The unit vector to rotate around.
	 * @param angle The angle, in radians, to rotate around the vector.
	 * @return The newly created quaternion.
	 */
	static function CreateFromAxisAngle(axis:cs.system.numerics.Vector3, angle:Single):cs.system.numerics.Quaternion;
	/**
	 * Creates a quaternion from the specified rotation matrix.
	 * @param matrix The rotation matrix.
	 * @return The newly created quaternion.
	 */
	static function CreateFromRotationMatrix(matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Quaternion;
	/**
	 * Creates a new quaternion from the given yaw, pitch, and roll.
	 * @param yaw The yaw angle, in radians, around the Y axis.
	 * @param pitch The pitch angle, in radians, around the X axis.
	 * @param roll The roll angle, in radians, around the Z axis.
	 * @return The resulting quaternion.
	 */
	static function CreateFromYawPitchRoll(yaw:Single, pitch:Single, roll:Single):cs.system.numerics.Quaternion;
	/**
	 * Divides one quaternion by a second quaternion.
	 * @param value1 The dividend.
	 * @param value2 The divisor.
	 * @return The quaternion that results from dividing  by .
	 */
	static function Divide(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Calculates the dot product of two quaternions.
	 * @param quaternion1 The first quaternion.
	 * @param quaternion2 The second quaternion.
	 * @return The dot product.
	 */
	static function Dot(quaternion1:cs.system.numerics.Quaternion, quaternion2:cs.system.numerics.Quaternion):Single;
	/**
	 * Returns the inverse of a quaternion.
	 * @param value The quaternion.
	 * @return The inverted quaternion.
	 */
	static function Inverse(value:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Performs a linear interpolation between two quaternions based on a value that
	 * specifies the weighting of the second quaternion.
	 * @param quaternion1 The first quaternion.
	 * @param quaternion2 The second quaternion.
	 * @param amount The relative weight of  in the interpolation.
	 * @return The interpolated quaternion.
	 */
	static function Lerp(quaternion1:cs.system.numerics.Quaternion, quaternion2:cs.system.numerics.Quaternion, amount:Single):cs.system.numerics.Quaternion;
	@:overload(function(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion {})
	/**
	 * Returns the quaternion that results from multiplying two quaternions together.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The product quaternion.
	 */
	static function Multiply(value1:cs.system.numerics.Quaternion, value2:Single):cs.system.numerics.Quaternion;
	/**
	 * Reverses the sign of each component of the quaternion.
	 * @param value The quaternion to negate.
	 * @return The negated quaternion.
	 */
	static function Negate(value:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Divides each component of a specified  by its length.
	 * @param value The quaternion to normalize.
	 * @return The normalized quaternion.
	 */
	static function Normalize(value:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Adds each element in one quaternion with its corresponding element in a second
	 * quaternion.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The quaternion that contains the summed values of  and .
	 */
	static function op_Addition(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Divides one quaternion by a second quaternion.
	 * @param value1 The dividend.
	 * @param value2 The divisor.
	 * @return The quaternion that results from dividing  by .
	 */
	static function op_Division(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Returns a value that indicates whether two quaternions are equal.
	 * @param value1 The first quaternion to compare.
	 * @param value2 The second quaternion to compare.
	 * @return if the two quaternions are equal; otherwise, .
	 */
	static function op_Equality(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):Bool;
	/**
	 * Returns a value that indicates whether two quaternions are not equal.
	 * @param value1 The first quaternion to compare.
	 * @param value2 The second quaternion to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):Bool;
	@:overload(function(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion {})
	/**
	 * Returns the quaternion that results from multiplying two quaternions together.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The product quaternion.
	 */
	static function op_Multiply(value1:cs.system.numerics.Quaternion, value2:Single):cs.system.numerics.Quaternion;
	/**
	 * Subtracts each element in a second quaternion from its corresponding element in
	 * a first quaternion.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The quaternion containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function op_Subtraction(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Reverses the sign of each component of the quaternion.
	 * @param value The quaternion to negate.
	 * @return The negated quaternion.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	/**
	 * Interpolates between two quaternions, using spherical linear interpolation.
	 * @param quaternion1 The first quaternion.
	 * @param quaternion2 The second quaternion.
	 * @param amount The relative weight of the second quaternion in the interpolation.
	 * @return The interpolated quaternion.
	 */
	static function Slerp(quaternion1:cs.system.numerics.Quaternion, quaternion2:cs.system.numerics.Quaternion, amount:Single):cs.system.numerics.Quaternion;
	/**
	 * Subtracts each element in a second quaternion from its corresponding element in
	 * a first quaternion.
	 * @param value1 The first quaternion.
	 * @param value2 The second quaternion.
	 * @return The quaternion containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function Subtract(value1:cs.system.numerics.Quaternion, value2:cs.system.numerics.Quaternion):cs.system.numerics.Quaternion;
	@:overload(function(other:cs.system.numerics.Quaternion):Bool {})
	/**
	 * Returns a value that indicates whether this instance and another quaternion are
	 * equal.
	 * @param other The other quaternion.
	 * @return if the two quaternions are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Calculates the length of the quaternion.
	 * @return The computed length of the quaternion.
	 */
	function Length():Single;
	/**
	 * Calculates the squared length of the quaternion.
	 * @return The length squared of the quaternion.
	 */
	function LengthSquared():Single;
	/**
	 * Returns a string that represents this quaternion.
	 * @return The string representation of this quaternion.
	 */
	function ToString():String;
}

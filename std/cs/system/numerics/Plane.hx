package cs.system.numerics;

/** Represents a plane in three-dimensional space. */
@:native("System.Numerics.Plane")
extern class Plane extends cs.system.ValueType {
	/** The distance of the plane along its normal from the origin. */
	var D:Single;
	/** The normal vector of the plane. */
	var Normal:cs.system.numerics.Vector3;
	@:overload(function(value:cs.system.numerics.Vector4):Void {})
	@:overload(function(normal:cs.system.numerics.Vector3, d:Single):Void {})
	function new(x:Single, y:Single, z:Single, d:Single):Void;
	/**
	 * Creates a  object that contains three specified points.
	 * @param point1 The first point defining the plane.
	 * @param point2 The second point defining the plane.
	 * @param point3 The third point defining the plane.
	 * @return The plane containing the three points.
	 */
	static function CreateFromVertices(point1:cs.system.numerics.Vector3, point2:cs.system.numerics.Vector3, point3:cs.system.numerics.Vector3):cs.system.numerics.Plane;
	/**
	 * Calculates the dot product of a plane and a 4-dimensional vector.
	 * @param plane The plane.
	 * @param value The four-dimensional vector.
	 * @return The dot product.
	 */
	static function Dot(plane:cs.system.numerics.Plane, value:cs.system.numerics.Vector4):Single;
	/**
	 * Returns the dot product of a specified three-dimensional vector and the normal
	 * vector of this plane plus the distance () value of the plane.
	 * @param plane The plane.
	 * @param value The 3-dimensional vector.
	 * @return The dot product.
	 */
	static function DotCoordinate(plane:cs.system.numerics.Plane, value:cs.system.numerics.Vector3):Single;
	/**
	 * Returns the dot product of a specified three-dimensional vector and the  vector
	 * of this plane.
	 * @param plane The plane.
	 * @param value The three-dimensional vector.
	 * @return The dot product.
	 */
	static function DotNormal(plane:cs.system.numerics.Plane, value:cs.system.numerics.Vector3):Single;
	/**
	 * Creates a new  object whose normal vector is the source plane's normal vector
	 * normalized.
	 * @param value The source plane.
	 * @return The normalized plane.
	 */
	static function Normalize(value:cs.system.numerics.Plane):cs.system.numerics.Plane;
	/**
	 * Returns a value that indicates whether two planes are equal.
	 * @param value1 The first plane to compare.
	 * @param value2 The second plane to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(value1:cs.system.numerics.Plane, value2:cs.system.numerics.Plane):Bool;
	/**
	 * Returns a value that indicates whether two planes are not equal.
	 * @param value1 The first plane to compare.
	 * @param value2 The second plane to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.numerics.Plane, value2:cs.system.numerics.Plane):Bool;
	@:overload(function(plane:cs.system.numerics.Plane, matrix:cs.system.numerics.Matrix4x4):cs.system.numerics.Plane {})
	/**
	 * Transforms a normalized plane by a 4x4 matrix.
	 * @param plane The normalized plane to transform.
	 * @param matrix The transformation matrix to apply to .
	 * @return The transformed plane.
	 */
	static function Transform(plane:cs.system.numerics.Plane, rotation:cs.system.numerics.Quaternion):cs.system.numerics.Plane;
	@:overload(function(other:cs.system.numerics.Plane):Bool {})
	/**
	 * Returns a value that indicates whether this instance and another plane object
	 * are equal.
	 * @param other The other plane.
	 * @return if the two planes are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of this plane object.
	 * @return A string that represents this  object.
	 */
	function ToString():String;
}

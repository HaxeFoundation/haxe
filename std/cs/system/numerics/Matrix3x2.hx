package cs.system.numerics;

/** Represents a 3x2 matrix. */
@:native("System.Numerics.Matrix3x2")
extern class Matrix3x2 extends cs.system.ValueType {
	/**
	 * Gets the multiplicative identity matrix.
	 * @return The multiplicative identify matrix.
	 */
	static var Identity(default, never):cs.system.numerics.Matrix3x2;
	/** The first element of the first row. */
	var M11:Single;
	/** The second element of the first row. */
	var M12:Single;
	/** The first element of the second row. */
	var M21:Single;
	/** The second element of the second row. */
	var M22:Single;
	/** The first element of the third row. */
	var M31:Single;
	/** The second element of the third row. */
	var M32:Single;
	/**
	 * Indicates whether the current matrix is the identity matrix.
	 * @return if the current matrix is the identity matrix; otherwise, .
	 */
	var IsIdentity(default, never):Bool;
	/**
	 * Gets or sets the translation component of this matrix.
	 * @return The translation component of the current instance.
	 */
	var Translation(default, default):cs.system.numerics.Vector2;
	function new(m11:Single, m12:Single, m21:Single, m22:Single, m31:Single, m32:Single):Void;
	/**
	 * Adds each element in one matrix with its corresponding element in a second
	 * matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix that contains the summed values of  and .
	 */
	static function Add(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	@:overload(function(radians:Single):cs.system.numerics.Matrix3x2 {})
	/**
	 * Creates a rotation matrix using the given rotation in radians.
	 * @param radians The amount of rotation, in radians.
	 * @return The rotation matrix.
	 */
	static function CreateRotation(radians:Single, centerPoint:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2;
	@:overload(function(scales:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2 {})
	@:overload(function(scale:Single):cs.system.numerics.Matrix3x2 {})
	@:overload(function(scales:cs.system.numerics.Vector2, centerPoint:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2 {})
	@:overload(function(scale:Single, centerPoint:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2 {})
	@:overload(function(xScale:Single, yScale:Single):cs.system.numerics.Matrix3x2 {})
	/**
	 * Creates a scaling matrix from the specified vector scale.
	 * @param scales The scale to use.
	 * @return The scaling matrix.
	 */
	static function CreateScale(xScale:Single, yScale:Single, centerPoint:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2;
	@:overload(function(radiansX:Single, radiansY:Single):cs.system.numerics.Matrix3x2 {})
	/**
	 * Creates a skew matrix from the specified angles in radians.
	 * @param radiansX The X angle, in radians.
	 * @param radiansY The Y angle, in radians.
	 * @return The skew matrix.
	 */
	static function CreateSkew(radiansX:Single, radiansY:Single, centerPoint:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2;
	@:overload(function(position:cs.system.numerics.Vector2):cs.system.numerics.Matrix3x2 {})
	/**
	 * Creates a translation matrix from the specified 2-dimensional vector.
	 * @param position The translation position.
	 * @return The translation matrix.
	 */
	static function CreateTranslation(xPosition:Single, yPosition:Single):cs.system.numerics.Matrix3x2;
	/**
	 * Inverts the specified matrix. The return value indicates whether the operation
	 * succeeded.
	 * @param matrix The matrix to invert.
	 * @param result When this method returns, contains the inverted matrix if the
	 * operation succeeded.
	 * @return if  was converted successfully; otherwise,  .
	 */
	static function Invert(matrix:cs.system.numerics.Matrix3x2, result:cs.Ref<cs.system.numerics.Matrix3x2>):Bool;
	/**
	 * Performs a linear interpolation from one matrix to a second matrix based on a
	 * value that specifies the weighting of the second matrix.
	 * @param matrix1 The first matrix.
	 * @param matrix2 The second matrix.
	 * @param amount The relative weighting of .
	 * @return The interpolated matrix.
	 */
	static function Lerp(matrix1:cs.system.numerics.Matrix3x2, matrix2:cs.system.numerics.Matrix3x2, amount:Single):cs.system.numerics.Matrix3x2;
	@:overload(function(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2 {})
	/**
	 * Returns the matrix that results from multiplying two matrices together.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The product matrix.
	 */
	static function Multiply(value1:cs.system.numerics.Matrix3x2, value2:Single):cs.system.numerics.Matrix3x2;
	/**
	 * Negates the specified matrix by multiplying all its values by -1.
	 * @param value The matrix to negate.
	 * @return The negated matrix.
	 */
	static function Negate(value:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	/**
	 * Adds each element in one matrix with its corresponding element in a second
	 * matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix that contains the summed values.
	 */
	static function op_Addition(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	/**
	 * Returns a value that indicates whether the specified matrices are equal.
	 * @param value1 The first matrix to compare.
	 * @param value2 The second matrix to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):Bool;
	/**
	 * Returns a value that indicates whether the specified matrices are not equal.
	 * @param value1 The first matrix to compare.
	 * @param value2 The second matrix to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):Bool;
	@:overload(function(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2 {})
	/**
	 * Returns the matrix that results from multiplying two matrices together.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The product matrix.
	 */
	static function op_Multiply(value1:cs.system.numerics.Matrix3x2, value2:Single):cs.system.numerics.Matrix3x2;
	/**
	 * Subtracts each element in a second matrix from its corresponding element in a
	 * first matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function op_Subtraction(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	/**
	 * Negates the specified matrix by multiplying all its values by -1.
	 * @param value The matrix to negate.
	 * @return The negated matrix.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	/**
	 * Subtracts each element in a second matrix from its corresponding element in a
	 * first matrix.
	 * @param value1 The first matrix.
	 * @param value2 The second matrix.
	 * @return The matrix containing the values that result from subtracting each
	 * element in  from its corresponding element in .
	 */
	static function Subtract(value1:cs.system.numerics.Matrix3x2, value2:cs.system.numerics.Matrix3x2):cs.system.numerics.Matrix3x2;
	@:overload(function(other:cs.system.numerics.Matrix3x2):Bool {})
	/**
	 * Returns a value that indicates whether this instance and another 3x2 matrix are
	 * equal.
	 * @param other The other matrix.
	 * @return if the two matrices are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Calculates the determinant for this matrix.
	 * @return The determinant.
	 */
	function GetDeterminant():Single;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents this matrix.
	 * @return The string representation of this matrix.
	 */
	function ToString():String;
}
